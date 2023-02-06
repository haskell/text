-- | Tests for encoding and decoding

{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tests.Properties.Transcoding
    ( testTranscoding
    ) where

import Prelude hiding (head, tail)
import Data.Bits ((.&.), shiftR)
import Data.Char (chr, ord)
import Data.Functor (void)
import Data.Maybe (isNothing)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif
import Data.Word (Word8)
import Test.QuickCheck hiding ((.&.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Tests.QuickCheckUtils
import qualified Control.Exception as Exception
import qualified Data.Bits as Bits (shiftL, shiftR)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import qualified Data.Text.Internal.Encoding as E
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL

t_ascii t    = E.decodeASCII (E.encodeUtf8 a) === a
    where a  = T.map (\c -> chr (ord c `mod` 128)) t
tl_ascii t   = EL.decodeASCII (EL.encodeUtf8 a) === a
    where a  = TL.map (\c -> chr (ord c `mod` 128)) t

t_latin1     = E.decodeLatin1 `eq` (T.pack . BC.unpack)
tl_latin1    = EL.decodeLatin1 `eq` (TL.pack . BLC.unpack)

t_p_utf8_1   = testValidateUtf8_ [0x63] 1
t_p_utf8_2   = testValidateUtf8_ [0x63, 0x63, 0x63] 3
t_p_utf8_3   = testValidateUtf8_ [0x63, 0x63, 0xc2, 0x80, 0x63] 5
t_p_utf8_4   = testValidateUtf8_ [0x63, 0xe1, 0x80, 0x80, 0x63] 5
t_p_utf8_5   = testValidateUtf8_ [0xF0, 0x90, 0x80, 0x80, 0x63] 5
t_p_utf8_6   = testValidateUtf8_ [0x63, 0x63, 0xF0, 0x90, 0x80] 2
t_p_utf8_7   = testValidateUtf8_ [0x63, 0x63, 0x63, 0xF0, 0x90] 3
t_p_utf8_8   = testValidateUtf8Fail [0xF0, 0x90, 0x80, 0x63, 0x63] 0
t_p_utf8_9   = testValidateUtf8Fail [0x63, 0x63, 0x80, 0x63, 0x63] 2
t_p_utf8_0   = testValidateUtf8Fail [0x63, 0x63, 0xe1, 0x63, 0x63] 2

testValidateUtf8With ::
  (B.ByteString -> (Int, Maybe E.Utf8State)) ->
  (Maybe E.Utf8State -> IO r) ->
  [Word8] -> Int -> IO r
testValidateUtf8With validate k xs expectedLen = case validate (B.pack xs) of
  (len, s) -> do
    len @?= expectedLen
    k s

expectJust :: Maybe a -> IO a
expectJust Nothing = assertFailure "Unexpected Nothing"
expectJust (Just s) = pure s

expectNothing :: Maybe a -> IO ()
expectNothing Nothing = pure ()
expectNothing (Just _) = assertFailure "Unexpected Just"

testValidateUtf8 :: [Word8] -> Int -> IO E.Utf8State
testValidateUtf8 = testValidateUtf8With E.validateUtf8Chunk expectJust

testValidateUtf8_ :: [Word8] -> Int -> IO ()
testValidateUtf8_ = testValidateUtf8With E.validateUtf8Chunk (void . expectJust)

testValidateUtf8Fail :: [Word8] -> Int -> IO ()
testValidateUtf8Fail = testValidateUtf8With E.validateUtf8Chunk expectNothing

testValidateUtf8More :: E.Utf8State -> [Word8] -> Int -> IO E.Utf8State
testValidateUtf8More s =  testValidateUtf8With (E.validateUtf8More s) expectJust

testValidateUtf8MoreFail :: E.Utf8State -> [Word8] -> Int -> IO ()
testValidateUtf8MoreFail s = testValidateUtf8With (E.validateUtf8More s) expectNothing

t_pn_utf8_1 = do
  s <- testValidateUtf8 [0xF0, 0x90, 0x80] 0
  _ <- testValidateUtf8More s [0x80] 1
  testValidateUtf8MoreFail s [0x7f] (-3)
t_pn_utf8_2 = do
  s0 <- testValidateUtf8 [0xF0] 0
  testValidateUtf8MoreFail s0 [0x7f] (-1)
  s1 <- testValidateUtf8More s0 [0x90] (-1)
  testValidateUtf8MoreFail s1 [0x7f] (-2)
  s2 <- testValidateUtf8More s1 [0x80] (-2)
  testValidateUtf8MoreFail s2 [0x7f] (-3)
  _ <- testValidateUtf8More s2 [0x80] 1
  pure ()
t_pn_utf8_3 = do
  s1 <- testValidateUtf8 [0xc2] 0
  assertBool "PartialUtf8 must be partial" $ B.length (E.getPartialUtf8 s1) < E.getCompleteLen s1
  testValidateUtf8MoreFail s1 [0x80, 0x80] 1

-- Precondition: (i, ms1) = E.validateUtf8More s chunk
--
-- The index points to the end of the longest valid prefix
-- of prechunk `B.append` chunk
pre_validateUtf8More_validPrefix s chunk i =
  let prechunk = E.getPartialUtf8 s in
  -- Note: i <= 0 implies take i = id
  let (j, ms2) = E.validateUtf8Chunk (B.take (B.length prechunk + i) (prechunk `B.append` chunk)) in
  counterexample (show prechunk) $
    (B.length prechunk + i, ms2) === (j, Just E.startUtf8State)

-- Precondition: (i, Nothing) = E.validateUtf8More s chunk
--
-- Appending to an invalid chunk yields another invalid chunk.
pre_validateUtf8More_maximalPrefix s chunk i more =
  E.validateUtf8More s (chunk `B.append` more) === (i, Nothing)

-- Precondition: (i, Just s1) = E.validateUtf8More s chunk
pre_validateUtf8More_suffix s chunk i s1 =
  if 0 <= i
  then B.drop i chunk === p2b s1         -- The state s1 contains a suffix of the chunk.
  else p2b s `B.append` chunk === p2b s1 -- Or the chunk extends the incomplete code point in s1.

-- Precondition: (i, Just s1) = E.validateUtf8More s chunk1
--
-- Validating two chunks sequentially is equivalent to validating them at once.
pre_validateUtf8More_append s chunk1 s1 chunk2 =
  let (j, ms2) = E.validateUtf8More s1 chunk2 in
  (B.length chunk1 + j, ms2) === E.validateUtf8More s (chunk1 `B.append` chunk2)

-- These wrappers use custom generators to satisfy the above properties.

t_validateUtf8More_validPrefix = property $ do
  cex@(s, chunk, i, _ms1) <- randomMoreChunk
  pure $ counterexample (show cex) $
    pre_validateUtf8More_validPrefix s chunk i

t_validateUtf8More_maximalPrefix = property $ do
  -- We want chunks that fail validation: force their size to be big,..
  cex@(s, chunk, i, ms1) <- scale (* 3) arbitraryMoreChunk
  pure $ counterexample (show cex) $
    -- ... and just use rejection sampling
    isNothing ms1 ==>
    pre_validateUtf8More_maximalPrefix s chunk i

t_validateUtf8More_valid = property $ do
  cex@(s, chunk1, i, s1, chunk2) <- validMoreChunks
  pure $ counterexample (show cex) $
    pre_validateUtf8More_suffix s chunk1 i s1 .&&.
    pre_validateUtf8More_append s chunk1 s1 chunk2

randomMoreChunk, arbitraryMoreChunk, validMoreChunk :: Gen (E.Utf8State, B.ByteString, Int, Maybe E.Utf8State)
randomMoreChunk = oneof [arbitraryMoreChunk, validMoreChunk]

arbitraryMoreChunk = do
  s <- randomUtf8State
  chunk <- arbitrary
  let (i, ms1) = E.validateUtf8More s chunk
  pure (s, chunk, i, ms1)

-- | Generate a random state by parsing a prefix of a Char
randomUtf8State :: Gen E.Utf8State
randomUtf8State = do
  c <- arbitrary
  chunk <- elements (B.inits (E.encodeUtf8 (T.singleton c)))
  case E.validateUtf8Chunk chunk of
    (_, Just s) -> pure s
    (_, Nothing) -> error "should not happen"

-- | Make a valid chunk, i.e., (s, chunk) such that
--
-- validateUtf8More s chunk = (i, Just s1)
--
-- Also returning i and s1 to not repeat work.
validMoreChunk = do
  (s, chunk, i, s1, _chunk2) <- validMoreChunks
  pure (s, chunk, i, Just s1)

-- | Make a valid chunk by slicing a valid UTF8 bs,
-- and also provide a second chunk which is a valid extension
-- with 0.5 probability.
validMoreChunks :: Gen (E.Utf8State, B.ByteString, Int, E.Utf8State, B.ByteString)
validMoreChunks = do
  bs <- E.encodeUtf8 <$> scale (* 3) arbitrary
  -- Take an intermediate state.
  -- No need to go too far since code points are at most 4 bytes long
  i <- choose (0, 3)
  let (bs0, bs1) = B.splitAt i bs
  case E.validateUtf8Chunk bs0 of
    (_, Just s) -> do
      j <- choose (0, B.length bs1)
      let (chunk1, chunk2') = B.splitAt j bs1
      case E.validateUtf8More s chunk1 of
        (n1, Just s1) -> do
          chunk2 <- oneof [pure chunk2', arbitrary]
          pure (s, chunk1, n1, s1, chunk2)
        (_, Nothing) -> error "should not happen"
    (_, Nothing) -> error "should not happen"

t_utf8_c     = (E.strictBuilderToText . fst3 . E.decodeUtf8Chunk . E.encodeUtf8) `eq` id
t_utf8       = (E.decodeUtf8 . E.encodeUtf8) `eq` id
t_utf8'      = (E.decodeUtf8' . E.encodeUtf8) `eq` (id . Right)
tl_utf8      = (EL.decodeUtf8 . EL.encodeUtf8) `eq` id
tl_utf8'     = (EL.decodeUtf8' . EL.encodeUtf8) `eq` (id . Right)
t_utf16LE    = (E.decodeUtf16LE . E.encodeUtf16LE) `eq` id
tl_utf16LE   = (EL.decodeUtf16LE . EL.encodeUtf16LE) `eq` id
t_utf16BE    = (E.decodeUtf16BE . E.encodeUtf16BE) `eq` id
tl_utf16BE   = (EL.decodeUtf16BE . EL.encodeUtf16BE) `eq` id
t_utf32LE    = (E.decodeUtf32LE . E.encodeUtf32LE) `eq` id
tl_utf32LE   = (EL.decodeUtf32LE . EL.encodeUtf32LE) `eq` id
t_utf32BE    = (E.decodeUtf32BE . E.encodeUtf32BE) `eq` id
tl_utf32BE   = (EL.decodeUtf32BE . EL.encodeUtf32BE) `eq` id

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

runBuilder :: B.Builder -> B.ByteString
runBuilder =
  -- Use smallish buffers to exercise bufferFull case as well
  BL.toStrict . B.toLazyByteStringWith (B.safeStrategy 5 5) ""

t_encodeUtf8Builder_ toBuilder = (runBuilder . toBuilder) `eq` E.encodeUtf8

t_encodeUtf8Builder_nonZeroOffset_ toBuilder (Positive n) =
  (runBuilder . toBuilder . T.drop n) `eq` (E.encodeUtf8 . T.drop n)

t_encodeUtf8Builder = t_encodeUtf8Builder_ E.encodeUtf8Builder
t_encodeUtf8Builder_nonZeroOffset = t_encodeUtf8Builder_nonZeroOffset_ E.encodeUtf8Builder

t_encodeUtf8BuilderEscaped = t_encodeUtf8Builder_ (E.encodeUtf8BuilderEscaped (BP.liftFixedToBounded BP.word8))
t_encodeUtf8BuilderEscaped_nonZeroOffset = t_encodeUtf8Builder_nonZeroOffset_ (E.encodeUtf8BuilderEscaped (BP.liftFixedToBounded BP.word8))

t_encodeUtf8Builder_sanity t =
  (runBuilder . E.encodeUtf8Builder) t ===
    (runBuilder . E.encodeUtf8BuilderEscaped (BP.liftFixedToBounded BP.word8)) t

t_utf8_incr (Positive n) =
  (T.concat . map fst . feedChunksOf n E.streamDecodeUtf8 . E.encodeUtf8) `eq` id

feedChunksOf :: Int -> (B.ByteString -> E.Decoding) -> B.ByteString
             -> [(T.Text, B.ByteString)]
feedChunksOf n f bs
  | B.null bs  = []
  | otherwise  = let (x,y) = B.splitAt n bs
                     E.Some t b f' = f x
                 in (t,b) : feedChunksOf n f' y

t_utf8_undecoded t =
  let b = E.encodeUtf8 t
      ls = concatMap (leftover . E.encodeUtf8 . T.singleton) . T.unpack $ t
      leftover = (++ [B.empty]) . init . drop 1 . B.inits
  in (map snd . feedChunksOf 1 E.streamDecodeUtf8) b === ls

data InvalidUtf8 = InvalidUtf8
  { iu8Prefix  :: T.Text
  , iu8Invalid :: B.ByteString
  , iu8Suffix  :: T.Text
  } deriving (Eq)

instance Show InvalidUtf8 where
  show i = "InvalidUtf8 {prefix = "  ++ show (iu8Prefix i)
                   ++ ", invalid = " ++ show (iu8Invalid i)
                   ++ ", suffix = "  ++ show (iu8Suffix i)
                   ++ ", asBS = "    ++ show (toByteString i)
                   ++ ", length = "  ++ show (B.length (toByteString i))
                   ++ "}"

toByteString :: InvalidUtf8 -> B.ByteString
toByteString (InvalidUtf8 a b c) =
  E.encodeUtf8 a `B.append` b `B.append` E.encodeUtf8 c

instance Arbitrary InvalidUtf8 where
  arbitrary = oneof
    [ InvalidUtf8 <$> pure mempty <*> genInvalidUTF8 <*> pure mempty
    , InvalidUtf8 <$> pure mempty <*> genInvalidUTF8 <*> arbitrary
    , InvalidUtf8 <$> arbitrary <*> genInvalidUTF8 <*> pure mempty
    , InvalidUtf8 <$> arbitrary <*> genInvalidUTF8 <*> arbitrary
    ]
  shrink (InvalidUtf8 a b c)
    =  map (\c' -> InvalidUtf8 a b c') (shrink c)
    ++ map (\a' -> InvalidUtf8 a' b c) (shrink a)

t_utf8_err :: InvalidUtf8 -> DecodeErr -> Property
t_utf8_err bad de = forAll (Blind <$> genDecodeErr de) $ \(Blind onErr) -> ioProperty $ do
  let decoded = E.decodeUtf8With onErr (toByteString bad)
      len = T.length (E.decodeUtf8With onErr (toByteString bad))
  l <- Exception.try (Exception.evaluate len)
  pure $ case l of
    Left (err :: Exception.SomeException) -> counterexample (show err) $
      length (show err) >= 0
    Right _  -> counterexample (show (decoded, l)) $ de /= Strict

t_utf8_err' :: B.ByteString -> Bool
t_utf8_err' bs = case E.decodeUtf8' bs of
  Left err -> length (show err) >= 0
  Right t  -> T.length t >= 0

genInvalidUTF8 :: Gen B.ByteString
genInvalidUTF8 = B.pack <$> oneof [
    -- invalid leading byte of a 2-byte sequence
    (:) <$> choose (0xC0, 0xC1) <*> upTo 1 contByte
    -- invalid leading byte of a 4-byte sequence
  , (:) <$> choose (0xF5, 0xFF) <*> upTo 3 contByte
    -- 4-byte sequence greater than U+10FFFF
  , do k <- choose (0x11, 0x13)
       let w0 = 0xF0 + (k `Bits.shiftR` 2)
           w1 = 0x80 + ((k .&. 3) `Bits.shiftL` 4)
       ([w0,w1]++) <$> vectorOf 2 contByte
    -- continuation bytes without a start byte
  , listOf1 contByte
    -- short 2-byte sequence
  , (:[]) <$> choose (0xC2, 0xDF)
    -- short 3-byte sequence
  , (:) <$> choose (0xE0, 0xEF) <*> upTo 1 contByte
    -- short 4-byte sequence
  , (:) <$> choose (0xF0, 0xF4) <*> upTo 2 contByte
    -- overlong encoding
  , do k <- choose (0 :: Int, 0xFFFF)
       case k of
         _ | k < 0x80   -> elements [ord2_ k, ord3_ k, ord4_ k]
           | k < 0x7FF  -> elements [ord3_ k, ord4_ k]
           | otherwise  -> return (ord4_ k)
  ]
  where
    contByte = (0x80 +) <$> choose (0, 0x3f)
    upTo n gen = do
      k <- choose (0,n)
      vectorOf k gen
    -- Data.Text.Internal.Encoding.Utf8.ord{2,3,4} without sanity checks
    ord2_ n = map fromIntegral [(n `shiftR` 6) + 0xC0, (n .&. 0x3F) + 0x80]
    ord3_ n = map fromIntegral [(n `shiftR` 12) + 0xE0, ((n `shiftR` 6) .&. 0x3F) + 0x80, (n .&. 0x3F) + 0x80]
    ord4_ n = map fromIntegral [(n `shiftR` 18) + 0xF0, ((n `shiftR` 12) .&. 0x3F) + 0x80, ((n `shiftR` 6) .&. 0x3F) + 0x80, (n .&. 0x3F) + 0x80]

decodeLL :: BL.ByteString -> TL.Text
decodeLL = EL.decodeUtf8With E.lenientDecode

decodeL :: B.ByteString -> T.Text
decodeL = E.decodeUtf8With E.lenientDecode

-- The lenient decoding of lazy bytestrings should not depend on how they are chunked,
-- and it should behave the same as decoding of strict bytestrings.
t_decode_utf8_lenient :: Property
t_decode_utf8_lenient = forAllShrinkShow arbitrary shrink (show . BL.toChunks) $ \bs ->
    decodeLL bs === (TL.fromStrict . decodeL . B.concat . BL.toChunks) bs

-- See http://unicode.org/faq/utf_bom.html#gen8
-- A sequence such as <110xxxxx2 0xxxxxxx2> is illegal ...
-- When faced with this illegal byte sequence ... a UTF-8 conformant process
-- must treat the first byte 110xxxxx2 as an illegal termination error
-- (e.g. filter it out or replace by 0xFFFD) ...
-- ... and continue processing at the second byte 0xxxxxxx2
t_decode_with_error2 =
  E.decodeUtf8With (\_ _ -> Just 'x') (B.pack [0xC2, 97]) === "xa"
t_decode_with_error3 =
  E.decodeUtf8With (\_ _ -> Just 'x') (B.pack [0xE0, 97, 97]) === "xaa"
t_decode_with_error4 =
  E.decodeUtf8With (\_ _ -> Just 'x') (B.pack [0xF0, 97, 97, 97]) === "xaaa"

t_decode_with_error1' = do
  E.Some x1 bs1 f1 <- pure $ E.streamDecodeUtf8With (\_ _ -> Just 'x') (B.pack [0xc2])
  x1 @?= ""
  bs1 @?= B.pack [0xc2]
  E.Some x2 bs2 _ <- pure $ f1 $ B.pack [0x80, 0x80]
  x2 @?= "\x80x"
  bs2 @?= mempty
t_decode_with_error2' =
  case E.streamDecodeUtf8With (\_ _ -> Just 'x') (B.pack [0xC2, 97]) of
    E.Some x _ _ -> x @?= "xa"
t_decode_with_error3' =
  case E.streamDecodeUtf8With (\_ _ -> Just 'x') (B.pack [0xC2, 97, 97]) of
    E.Some x _ _ -> x @?= "xaa"
t_decode_with_error4' =
  case E.streamDecodeUtf8With (\_ _ -> Just 'x') (B.pack [0xC2, 97, 97, 97]) of
    E.Some x _ _ -> x @?= "xaaa"
t_decode_with_error5' = do
  ret <- Exception.try $ Exception.evaluate $ E.streamDecodeUtf8 (B.pack [0x81])
  case ret of
    Left (_ :: E.UnicodeException) -> pure ()
    Right{} -> assertFailure "Unexpected success"

testDecodeUtf8With :: (Maybe E.Utf8State -> IO r) -> E.Utf8State -> [Word8] -> T.Text -> IO r
testDecodeUtf8With k s xs expected =
  let xs' = B.pack xs in
  case E.decodeUtf8More s xs' of
    (prefix, bs, s') -> do
      let txt = E.strictBuilderToText prefix
      txt @?= expected
      if T.null txt then
        bs @?= xs'
      else
        E.encodeUtf8 txt `B.append` bs @?= E.getPartialUtf8 s `B.append` xs'
      k s'

testDecodeUtf8 :: E.Utf8State -> [Word8] -> T.Text -> IO E.Utf8State
testDecodeUtf8 = testDecodeUtf8With (\ms -> case ms of
  Just s -> pure s
  Nothing -> assertFailure "Unexpected failure")

testDecodeUtf8Fail :: E.Utf8State -> [Word8] -> T.Text -> IO ()
testDecodeUtf8Fail = testDecodeUtf8With (\ms -> case ms of
  Just _ -> assertFailure "Unexpected failure"
  Nothing -> pure ())

t_decode_chunk1 = do
  s1 <- testDecodeUtf8 E.startUtf8State [0xc2] ""
  B.length (E.getPartialUtf8 s1) @?= 1
  testDecodeUtf8Fail s1 [0x80, 0x80] "\128"

t_decode_chunk2 = do
  s1 <- testDecodeUtf8 E.startUtf8State [0xf0] ""
  s2 <- testDecodeUtf8 s1 [0x90, 0x80] ""
  _  <- testDecodeUtf8 s2 [0x80, 0x41] "\65536A"
  pure ()

t_infix_concat bs1 text bs2 =
  forAll (Blind <$> genDecodeErr Replace) $ \(Blind onErr) ->
  text `T.isInfixOf`
    E.decodeUtf8With onErr (B.concat [bs1, E.encodeUtf8 text, bs2])

t_textToStrictBuilder =
  (E.strictBuilderToText . E.textToStrictBuilder) `eq` id

-- decodeUtf8Chunk splits the input bytestring
t_decodeUtf8Chunk_split chunk =
  let (pre, suf, _ms) = E.decodeUtf8Chunk chunk
  in s2b pre `B.append` suf === chunk

-- decodeUtf8More mostly splits the input bytestring,
-- also inserting bytes from the partial code point in s.
--
-- This is wrapped by t_decodeUtf8More_split to have more
-- likely valid chunks.
t_decodeUtf8More_split' s chunk =
  let (pre, suf, _ms) = E.decodeUtf8More s chunk
  in if B.length chunk > B.length suf
  then s2b pre `B.append` suf === p2b s `B.append` chunk
  else suf === chunk

-- The output state of decodeUtf8More contains the suffix.
--
-- Precondition (valid chunk): ms = Just s'
pre_decodeUtf8More_suffix s chunk =
  let (_pre, suf, ms) = E.decodeUtf8More s chunk
  in case ms of
    Nothing -> discard
    Just s' -> if B.length chunk > B.length suf
      then p2b s' === suf
      else p2b s' === p2b s `B.append` suf

-- Decoding chunks separately is equivalent to decoding their concatenation.
pre_decodeUtf8More_append s chunk1 chunk2 =
  let (pre1, _, ms1) = E.decodeUtf8More s chunk1 in
  case ms1 of
    Nothing -> discard
    Just s1 ->
      let (pre2, _, ms2) = E.decodeUtf8More s1 chunk2 in
      let (pre3, _, ms3) = E.decodeUtf8More s (chunk1 `B.append` chunk2) in
      (s2b (pre1 <> pre2), ms2) === (s2b pre3, ms3)

-- Properties for any chunk
-- (but do try to generate valid chunks often enough)
t_decodeUtf8More1 = property $ do
  cex@(s, chunk, _, _) <- randomMoreChunk
  pure $ counterexample (show cex) $
    t_decodeUtf8More_split' s chunk

-- Properties that require valid chunks
t_decodeUtf8More2 = property $ do
  cex@(s, chunk, _, _, chunk2) <- validMoreChunks
  pure $ counterexample (show cex) $
    pre_decodeUtf8More_suffix s chunk .&&.
    pre_decodeUtf8More_append s chunk chunk2

s2b = E.encodeUtf8 . E.strictBuilderToText
p2b = E.getPartialUtf8

testTranscoding :: TestTree
testTranscoding =
  testGroup "transcoding" [
    testProperty "t_ascii" t_ascii,
    testProperty "tl_ascii" tl_ascii,
    testProperty "t_latin1" t_latin1,
    testProperty "tl_latin1" tl_latin1,
    testProperty "t_utf8" t_utf8,
    testProperty "t_utf8'" t_utf8',
    testProperty "t_utf8_undecoded" t_utf8_undecoded,
    testProperty "t_utf8_incr" t_utf8_incr,
    testProperty "tl_utf8" tl_utf8,
    testProperty "tl_utf8'" tl_utf8',
    testProperty "t_utf16LE" t_utf16LE,
    testProperty "tl_utf16LE" tl_utf16LE,
    testProperty "t_utf16BE" t_utf16BE,
    testProperty "tl_utf16BE" tl_utf16BE,
    testProperty "t_utf32LE" t_utf32LE,
    testProperty "tl_utf32LE" tl_utf32LE,
    testProperty "t_utf32BE" t_utf32BE,
    testProperty "tl_utf32BE" tl_utf32BE,
    testGroup "builder" [
      testProperty "t_encodeUtf8Builder" t_encodeUtf8Builder,
      testProperty "t_encodeUtf8Builder_nonZeroOffset" t_encodeUtf8Builder_nonZeroOffset,
      testProperty "t_encodeUtf8BuilderEscaped" t_encodeUtf8BuilderEscaped,
      testProperty "t_encodeUtf8BuilderEscaped_nonZeroOffset" t_encodeUtf8BuilderEscaped_nonZeroOffset,
      testProperty "t_encodeUtf8Builder_sanity" t_encodeUtf8Builder_sanity
    ],
    testGroup "errors" [
      testProperty "t_utf8_err" t_utf8_err,
      testProperty "t_utf8_err'" t_utf8_err'
    ],
    testGroup "error recovery" [
      testProperty "t_decode_utf8_lenient" t_decode_utf8_lenient,
      testProperty "t_decode_with_error2" t_decode_with_error2,
      testProperty "t_decode_with_error3" t_decode_with_error3,
      testProperty "t_decode_with_error4" t_decode_with_error4,
      testCase "t_decode_with_error1'" t_decode_with_error1',
      testCase "t_decode_with_error2'" t_decode_with_error2',
      testCase "t_decode_with_error3'" t_decode_with_error3',
      testCase "t_decode_with_error4'" t_decode_with_error4',
      testCase "t_decode_with_error5'" t_decode_with_error5',
      testProperty "t_infix_concat" t_infix_concat
    ],
    testGroup "validate" [
      testProperty "t_validateUtf8More_validPrefix" t_validateUtf8More_validPrefix,
      testProperty "t_validateUtf8More_maximalPrefix" t_validateUtf8More_maximalPrefix,
      testProperty "t_validateUtf8More_valid" t_validateUtf8More_valid
    ],
    testGroup "streaming" [
      testProperty "t_utf8_c" t_utf8_c,
      testCase "t_p_utf8_1" t_p_utf8_1,
      testCase "t_p_utf8_2" t_p_utf8_2,
      testCase "t_p_utf8_3" t_p_utf8_3,
      testCase "t_p_utf8_4" t_p_utf8_4,
      testCase "t_p_utf8_5" t_p_utf8_5,
      testCase "t_p_utf8_6" t_p_utf8_6,
      testCase "t_p_utf8_7" t_p_utf8_7,
      testCase "t_p_utf8_8" t_p_utf8_8,
      testCase "t_p_utf8_9" t_p_utf8_9,
      testCase "t_p_utf8_0" t_p_utf8_0,
      testCase "t_pn_utf8_1" t_pn_utf8_1,
      testCase "t_pn_utf8_2" t_pn_utf8_2,
      testCase "t_pn_utf8_3" t_pn_utf8_3,
      testCase "t_decode_chunk1" t_decode_chunk1,
      testCase "t_decode_chunk2" t_decode_chunk2,
      testProperty "t_decodeUtf8Chunk_split" t_decodeUtf8Chunk_split,
      testProperty "t_decodeUtf8More1" t_decodeUtf8More1,
      testProperty "t_decodeUtf8More2" t_decodeUtf8More2
    ],
    testGroup "strictBuilder" [
      testProperty "textToStrictBuilder" t_textToStrictBuilder
    ]
  ]
