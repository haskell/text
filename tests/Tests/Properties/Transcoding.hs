-- | Tests for encoding and decoding

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tests.Properties.Transcoding
    ( testTranscoding
    ) where

import Data.Bits ((.&.), shiftR)
import Data.Char (chr, ord)
import Test.QuickCheck hiding ((.&.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
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
import qualified Data.Text.Encoding.Common as C
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL

t_asciiE t    = E.decodeAsciiE (E.encodeUtf8 a) === Right a
    where a  = T.map (\c -> chr (ord c `mod` 128)) t
tl_asciiE t    = EL.decodeAsciiE (EL.encodeUtf8 a) === Right a
    where a  = TL.map (\c -> chr (ord c `mod` 128)) t

t_ascii t    = E.decodeASCII (E.encodeUtf8 a) === a
    where a  = T.map (\c -> chr (ord c `mod` 128)) t
tl_ascii t   = EL.decodeASCII (EL.encodeUtf8 a) === a
    where a  = TL.map (\c -> chr (ord c `mod` 128)) t

t_latin1     = E.decodeLatin1 `eq` (T.pack . BC.unpack)
tl_latin1    = EL.decodeLatin1 `eq` (TL.pack . BLC.unpack)

whenEqProp a b next = if a == b
  then next
  else a === b

t_utf8       = (E.decodeUtf8 . E.encodeUtf8) `eq` id
t_utf8'      = (E.decodeUtf8' . E.encodeUtf8) `eq` (id . Right)
t_utf8_c     = (\ t ->
                let E.DecodeResult t' mC bs _ = E.decodeUtf8Chunks mempty $ E.encodeUtf8 t in
                (t', mC, bs)) `eq` (\ t -> (t, Nothing, mempty))
tl_utf8      = (EL.decodeUtf8 . EL.encodeUtf8) `eq` id
tl_utf8'     = (EL.decodeUtf8' . EL.encodeUtf8) `eq` (id . Right)
tl_utf8_c    = (\ t ->
                let E.DecodeResult t' mC bs _ = EL.decodeUtf8Chunks mempty $ EL.encodeUtf8 t in
                (t', mC, bs)) `eq` (\ t -> (t, Nothing, mempty))
t_utf16LE    = (E.decodeUtf16LE . E.encodeUtf16LE) `eq` id
t_utf16LE_c  = (\ t ->
                let E.DecodeResult t' mC bs _ = E.decodeUtf16Chunks False mempty $ E.encodeUtf16LE t in
                (t', mC, bs)) `eq` (\ t -> (t, Nothing, mempty))
tl_utf16LE   = (EL.decodeUtf16LE . EL.encodeUtf16LE) `eq` id
tl_utf16LE_c = (\ t ->
                let E.DecodeResult t' mC bs _ = EL.decodeUtf16Chunks False mempty $ EL.encodeUtf16LE t in
                (t', mC, bs)) `eq` (\ t -> (t, Nothing, mempty))
t_utf16BE    = (E.decodeUtf16BE . E.encodeUtf16BE) `eq` id
t_utf16BE_c  = (\ t ->
                let E.DecodeResult t' mC bs _ = E.decodeUtf16Chunks True mempty $ E.encodeUtf16BE t in
                (t', mC, bs)) `eq` (\ t -> (t, Nothing, mempty))
tl_utf16BE   = (EL.decodeUtf16BE . EL.encodeUtf16BE) `eq` id
tl_utf16BE_c = (\ t ->
                let E.DecodeResult t' mC bs _ = EL.decodeUtf16Chunks True mempty $ EL.encodeUtf16BE t in
                (t', mC, bs)) `eq` (\ t -> (t, Nothing, mempty))
t_utf32LE    = (E.decodeUtf32LE . E.encodeUtf32LE) `eq` id
t_utf32LE_c  = (\ t ->
                let E.DecodeResult t' mC bs _ = E.decodeUtf32Chunks False mempty $ E.encodeUtf32LE t in
                (t', mC, bs)) `eq` (\ t -> (t, Nothing, mempty))
tl_utf32LE   = (EL.decodeUtf32LE . EL.encodeUtf32LE) `eq` id
tl_utf32LE_c = (\ t ->
                let E.DecodeResult t' mC bs _ = EL.decodeUtf32Chunks False mempty $ EL.encodeUtf32LE t in
                (t', mC, bs)) `eq` (\ t -> (t, Nothing, mempty))
t_utf32BE    = (E.decodeUtf32BE . E.encodeUtf32BE) `eq` id
t_utf32BE_c  = (\ t ->
                let E.DecodeResult t' mC bs _ = E.decodeUtf32Chunks True mempty $ E.encodeUtf32BE t in
                (t', mC, bs)) `eq` (\ t -> (t, Nothing, mempty))
tl_utf32BE   = (EL.decodeUtf32BE . EL.encodeUtf32BE) `eq` id
tl_utf32BE_c  = (\ t ->
                let E.DecodeResult t' mC bs _ = EL.decodeUtf32Chunks True mempty $ EL.encodeUtf32BE t in
                (t', mC, bs)) `eq` (\ t -> (t, Nothing, mempty))

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
      leftover = (++ [B.empty]) . init . tail . B.inits
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

t_utf8_c_err :: InvalidUtf8 -> Property
t_utf8_c_err bad =
  let E.DecodeResult t mW bs _ = E.decodeUtf8Chunks mempty $ toByteString bad in
  case mW of
    Just w -> counterexample (show w) True
    _  -> counterexample (show t) $ B.length bs > 0

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

t_chunk_decode_utf8_1 =
  let decodeResult0 = E.decodeUtf8Chunks mempty $ B.pack [0x68, 0x69, 0x2C, 0x20, 0xe2, 0x98, 0x83, 0x21] in
  decodeResult0 === E.DecodeResult "hi, ☃!" Nothing mempty 8
t_chunk_decode_utf8_2 =
  let decode = E.decodeUtf8Chunks mempty
      decodeResult0 = decode $ B.pack [97, 0xC2, 97]
      expectedBs0 = B.singleton 97
  in
  whenEqProp decodeResult0 (E.DecodeResult (T.singleton 'a') (Just 0xC2) expectedBs0 2) $
    let decodeResult1 = decode expectedBs0 in
    decodeResult1 === E.DecodeResult (T.singleton 'a') Nothing mempty 1
t_chunk_decode_utf8_3 =
  let decode = E.decodeUtf8Chunks
      decodeResult0 = decode mempty $ B.pack [104, 105, 32, 0xe2]
      expectedBs0 = B.singleton 0xe2
  in -- hi \xe2
  whenEqProp decodeResult0 (E.DecodeResult "hi " Nothing expectedBs0 3) $
    let decodeResult1 = decode expectedBs0 $ B.singleton 0x98
        expectedBs1 = B.pack [0xe2, 0x98]
    in
    whenEqProp decodeResult1 (E.DecodeResult "" Nothing expectedBs1 0) $
      let decodeResult2 = decode expectedBs1 $ B.pack [0x83, 32, 0xFF] in
      decodeResult2 === E.DecodeResult "☃ " (Just 0xFF) mempty 5

t_chunk_decode_utf16BE =
  let decode = E.decodeUtf16Chunks True
      expectedBs0 = B.pack [0]
      decodeResult0 = decode mempty expectedBs0 in
  whenEqProp decodeResult0 (E.DecodeResult T.empty Nothing expectedBs0 0) $
    let decodeResult1 = decode expectedBs0 $ B.pack [104, 0, 105, 0, 32, 0xD8, 0x01]
        expectedBs1 = B.pack [0xD8, 0x01]
    in
    whenEqProp decodeResult1 (E.DecodeResult "hi " Nothing expectedBs1 6) $
      let decodeResult2 = decode expectedBs1 $ B.pack [0xDC, 0x37, 0, 32, 0xDC, 0] in
      decodeResult2 === E.DecodeResult "\x10437 " (Just 0xDC00) mempty 8
t_chunk_decode_utf16LE =
  let decode = E.decodeUtf16Chunks False
      expectedBs0 = B.pack [104]
      decodeResult0 = decode mempty expectedBs0 in
  whenEqProp decodeResult0 (E.DecodeResult T.empty Nothing expectedBs0 0) $
    let decodeResult1 = decode expectedBs0 $ B.pack [0, 105, 0, 32, 0, 0x01, 0xD8]
        expectedBs1 = B.pack [0x01, 0xD8]
    in
    whenEqProp decodeResult1 (E.DecodeResult "hi " Nothing expectedBs1 6) $
      let decodeResult2 = decode expectedBs1 $ B.pack [0x37, 0xDC, 32, 0, 0, 0xDC] in
      decodeResult2 === E.DecodeResult "\x10437 " (Just 0xDC) mempty 8

t_chunk_decode_utf32BE =
  let decode = E.decodeUtf32Chunks True
      expBs0 = B.pack [0, 0]
      decodeResult0 = decode mempty $ B.pack [0, 0, 0, 104, 0, 0, 0, 105, 0, 0]
  in
  whenEqProp decodeResult0 (E.DecodeResult "hi" Nothing expBs0 8) $
    let expBs1 = B.pack [0, 0, 0x26]
        decodeResult1 = decode expBs0 $ B.pack [0, 32, 0, 0, 0x26]
    in
    whenEqProp decodeResult1 (E.DecodeResult " " Nothing expBs1 4) $
      let expBs2 = mempty
          decodeResult2 = decode expBs1 $ B.pack [0x03, 0, 0, 0, 32, 0, 0, 0xD8, 0]
      in
      decodeResult2 === E.DecodeResult "☃ " (Just 0xD800) expBs2 12
t_chunk_decode_utf32LE =
  let decode = E.decodeUtf32Chunks False
      expBs0 = B.pack [0x20, 0]
      decodeResult0 = decode mempty $ B.pack [104, 0, 0, 0, 105, 0, 0, 0, 0x20, 0]
  in
  whenEqProp decodeResult0 (E.DecodeResult "hi" Nothing expBs0 8) $
    let expBs1 = B.pack [0x03, 0x26, 0]
        decodeResult1 = decode expBs0 $ B.pack [0, 0, 0x03, 0x26, 0]
    in
    whenEqProp decodeResult1 (E.DecodeResult " " Nothing expBs1 4) $
      let expBs2 = mempty
          decodeResult2 = decode expBs1 $ B.pack [0, 32, 0, 0, 0, 0, 0xD8, 0, 0]
      in
      decodeResult2 === E.DecodeResult "☃ " (Just 0xD80000) expBs2 12

decodeLL :: BL.ByteString -> TL.Text
decodeLL = EL.decodeUtf8With C.lenientDecode

decodeL :: B.ByteString -> T.Text
decodeL = E.decodeUtf8With C.lenientDecode

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

t_decode_with_error2' =
  case E.streamDecodeUtf8With (\_ _ -> Just 'x') (B.pack [0xC2, 97]) of
    E.Some x _ _ -> x === "xa"
t_decode_with_error3' =
  case E.streamDecodeUtf8With (\_ _ -> Just 'x') (B.pack [0xC2, 97, 97]) of
    E.Some x _ _ -> x === "xaa"
t_decode_with_error4' =
  case E.streamDecodeUtf8With (\_ _ -> Just 'x') (B.pack [0xC2, 97, 97, 97]) of
    E.Some x _ _ -> x === "xaaa"
t_decode_with_error5' = ioProperty $ do
  ret <- Exception.try $ Exception.evaluate $ E.streamDecodeUtf8 (B.pack [0x81])
  pure $ case ret of
    Left (_ :: C.UnicodeException) -> True
    Right{} -> False

t_infix_concat bs1 text bs2 =
  forAll (Blind <$> genDecodeErr Replace) $ \(Blind onErr) ->
  text `T.isInfixOf`
    E.decodeUtf8With onErr (B.concat [bs1, E.encodeUtf8 text, bs2])

testTranscoding :: TestTree
testTranscoding =
  testGroup "transcoding" [
    testProperty "t_asciiE" t_asciiE,
    testProperty "tl_asciiE" tl_asciiE,
    testProperty "t_ascii" t_ascii,
    testProperty "tl_ascii" tl_ascii,
    testProperty "t_latin1" t_latin1,
    testProperty "tl_latin1" tl_latin1,
    testProperty "t_utf8" t_utf8,
    testProperty "t_utf8'" t_utf8',
    testProperty "t_utf8_c" t_utf8_c,
    testProperty "t_utf8_incr" t_utf8_incr,
    testProperty "t_utf8_undecoded" t_utf8_undecoded,
    testProperty "tl_utf8" tl_utf8,
    testProperty "tl_utf8'" tl_utf8',
    testProperty "tl_utf8_c" tl_utf8_c,
    testProperty "t_utf16LE" t_utf16LE,
    testProperty "t_utf16LE_c" t_utf16LE_c,
    testProperty "tl_utf16LE" tl_utf16LE,
    testProperty "tl_utf16LE_c" tl_utf16LE_c,
    testProperty "t_utf16BE" t_utf16BE,
    testProperty "t_utf16BE_c" t_utf16BE_c,
    testProperty "tl_utf16BE" tl_utf16BE,
    testProperty "tl_utf16BE_c" tl_utf16BE_c,
    testProperty "t_utf32LE" t_utf32LE,
    testProperty "t_utf32LE_c" t_utf32LE_c,
    testProperty "tl_utf32LE" tl_utf32LE,
    testProperty "tl_utf32LE_c" tl_utf32LE_c,
    testProperty "t_utf32BE" t_utf32BE,
    testProperty "t_utf32BE_c" t_utf32BE_c,
    testProperty "tl_utf32BE" tl_utf32BE,
    testProperty "tl_utf32BE_c" tl_utf32BE_c,
    testGroup "builder" [
      testProperty "t_encodeUtf8Builder" t_encodeUtf8Builder,
      testProperty "t_encodeUtf8Builder_nonZeroOffset" t_encodeUtf8Builder_nonZeroOffset,
      testProperty "t_encodeUtf8BuilderEscaped" t_encodeUtf8BuilderEscaped,
      testProperty "t_encodeUtf8BuilderEscaped_nonZeroOffset" t_encodeUtf8BuilderEscaped_nonZeroOffset,
      testProperty "t_encodeUtf8Builder_sanity" t_encodeUtf8Builder_sanity
    ],
    testGroup "errors" [
      testProperty "t_utf8_err" t_utf8_err,
      testProperty "t_utf8_c_err" t_utf8_c_err,
      testProperty "t_utf8_err'" t_utf8_err'
    ],
    testGroup "error recovery" [
      testProperty "t_chunk_decode_utf8_1" t_chunk_decode_utf8_1,
      testProperty "t_chunk_decode_utf8_2" t_chunk_decode_utf8_2,
      testProperty "t_chunk_decode_utf8_3" t_chunk_decode_utf8_3,
      testProperty "t_chunk_decode_utf16BE" t_chunk_decode_utf16BE,
      testProperty "t_chunk_decode_utf16LE" t_chunk_decode_utf16LE,
      testProperty "t_chunk_decode_utf32BE" t_chunk_decode_utf32BE,
      testProperty "t_chunk_decode_utf32LE" t_chunk_decode_utf32LE,
      testProperty "t_decode_utf8_lenient" t_decode_utf8_lenient,
      testProperty "t_decode_with_error2" t_decode_with_error2,
      testProperty "t_decode_with_error3" t_decode_with_error3,
      testProperty "t_decode_with_error4" t_decode_with_error4,
      testProperty "t_decode_with_error2'" t_decode_with_error2',
      testProperty "t_decode_with_error3'" t_decode_with_error3',
      testProperty "t_decode_with_error4'" t_decode_with_error4',
      testProperty "t_decode_with_error5'" t_decode_with_error5',
      testProperty "t_infix_concat" t_infix_concat
    ]
  ]
