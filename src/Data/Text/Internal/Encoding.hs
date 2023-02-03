{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
-- |
module Data.Text.Internal.Encoding where

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.Bits ((.&.), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Text.Internal (Text(..), empty, safe)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word32, Word8)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeElemOff)
import Data.Text.Internal.ByteStringCompat (withBS)
import Data.Text.Internal.Encoding.Utf8
  (Utf8CodePointState, utf8StartState, updateUtf8State, isUtf8StateIsComplete, utf8Length)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Unsafe.Char as Char
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

#ifdef SIMDUTF
import Foreign.C.Types (CInt(..))
#elif !MIN_VERSION_bytestring(0,11,2)
import qualified Data.ByteString.Unsafe as B
#endif

-- | State of decoding a 'ByteString' in UTF-8.
-- It consists of a value representing whether or
-- not the last byte is a complete code point, and on incompletion what
-- the 1 to 3 end bytes are that make up the incomplete code point.
data Utf8State = Utf8State
  { -- | Current UTF-8 code point state of the 'ByteString's
    -- that have been validated thus far.
    utf8CodePointState :: {-# UNPACK #-} !Utf8CodePointState
    -- | Get the incomplete UTF-8 code point of the 'ByteString's that
    -- have been validated thus far. The first byte of the 'Word32'
    -- indicates the number of bytes of the code point are available,
    -- and is followed by the bytes of the code point.
  , partialUtf8CodePoint :: {-# UNPACK #-} !PartialUtf8CodePoint
  }
  deriving (Eq, Ord, Show)

-- | This represtents the starting state of a UTF-8 validation check.
startUtf8State :: Utf8State 
startUtf8State = Utf8State utf8StartState partUtf8Empty

-- | Prefix of a valid UTF-8 encoded code point encoded in 4 bytes.
-- This consists of a length (in bytes) between 1 and 3 stored in the most
-- significant byte, and the actual bytes in the rest of the word.
--
-- All of its operations are the functions below.
-- The constructor should never be used outside of those.
newtype PartialUtf8CodePoint = PartialUtf8CodePoint Word32
  deriving (Eq, Ord, Show)

-- | Empty prefix.
partUtf8Empty :: PartialUtf8CodePoint
partUtf8Empty = PartialUtf8CodePoint 0

-- | Length of the partial code point, stored in the most significant byte.
partUtf8Len :: PartialUtf8CodePoint -> Int
partUtf8Len (PartialUtf8CodePoint w) = fromIntegral $ w `shiftR` 24

partUtf8CompleteLen :: PartialUtf8CodePoint -> Int
partUtf8CompleteLen c@(PartialUtf8CodePoint w)
  | partUtf8Len c == 0 = 0
  | 0xf0 <= firstByte = 4
  | 0xe0 <= firstByte = 3
  | 0xc2 <= firstByte = 2
  | otherwise = 0
  where
    firstByte = (w `shiftR` 16) .&. 255

-- | Get the @n@-th byte, assuming it is within bounds: @0 <= n < partUtf8Len c@.
partUtf8UnsafeIndex ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  PartialUtf8CodePoint -> Int -> Word8
partUtf8UnsafeIndex (PartialUtf8CodePoint w) n =
#if defined(ASSERTS)
  assert (0 <= n && n < partUtf8Len w) $
#endif
  fromIntegral $ w `shiftR` (16 - 8 * n)

partUtf8UnsafeAppend ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  PartialUtf8CodePoint -> ByteString -> PartialUtf8CodePoint
partUtf8UnsafeAppend c@(PartialUtf8CodePoint word) bs =
#if defined(ASSERTS)
  assert (lenc + lenbs <= 3) $
#endif
  PartialUtf8CodePoint $
    tryPush 0 $ tryPush 1 $ tryPush 2 $ word + (fromIntegral lenbs `shiftL` 24)
  where
    lenc = partUtf8Len c
    lenbs = B.length bs
    tryPush i w =
      if i < lenbs
      then w + (fromIntegral (B.index bs i) `shiftL` fromIntegral (16 - 8 * (lenc + i)))
      else w

-- | This avoids recursion to unfold to straightline code.
partUtf8Foldr :: (Word8 -> a -> a) -> a -> PartialUtf8CodePoint -> a
partUtf8Foldr f x0 c = case partUtf8Len c of
    0 -> x0
    1 -> build 0 x0
    2 -> build 0 (build 1 x0)
    _ -> build 0 (build 1 (build 2 x0))
  where
    build i x = f (partUtf8UnsafeIndex c i) x

partUtf8ToByteString :: PartialUtf8CodePoint -> B.ByteString
partUtf8ToByteString c = BI.unsafeCreate (partUtf8Len c) $ \ptr ->
  partUtf8Foldr (\w k i -> pokeElemOff ptr i w >> k (i+1)) (\_ -> pure ()) c 0

#ifdef SIMDUTF
foreign import ccall unsafe "_hs_text_is_valid_utf8" c_is_valid_utf8
    :: Ptr Word8 -> CSize -> IO CInt
#endif

-- | Validate a 'ByteString' as a UTF-8-encoded text.
--
-- @validateUtf8More chunk = (n, es)@
--
-- This function returns two values:
--
-- * The value 'n' indicates the longest prefix of the 'ByteString'
--   that is valid UTF-8-encoded data.
-- * The value 'es' indicates whether the 'ByteString'
--
--     * (@Left p@) contains an invalid code point and where the next
--       (potentially valid) code point begins, so that @p - n@ is the
--       number of invalid bytes, or
--     * (@Right s@) is valid, and all of the remaining bytes starting
--       at inbex 'n' are the beginning of an incomplete UTF-8 code
--       point, and 's' is the resulting 'Utf8State' value, which
--       can be used to validate against a following 'ByteString' with
--       'validateNextUtf8Chunk'.
validateUtf8Chunk :: ByteString -> (Int, Maybe Utf8State)
validateUtf8Chunk = validateUtf8ChunkFrom 0

-- | Add an offset to the index returned by 'validateUtf8More'.
--
-- @
-- validateUtf8ChunkFrom n = first (+ 1) . 'validateUtf8More' . 'B.drop' n
-- @
validateUtf8ChunkFrom :: Int -> ByteString -> (Int, Maybe Utf8State)
validateUtf8ChunkFrom ofs bs
#if defined(SIMDUTF) || MIN_VERSION_bytestring(0,11,2)
  | guessUtf8Boundary > 0 &&
    -- the rest of the bytestring valid utf-8 up to the boundary
    (
#ifdef SIMDUTF
      withBS (B.drop ofs bs) $ \ fp _ -> unsafeDupablePerformIO $
        unsafeWithForeignPtr fp $ \ptr -> (/= 0) <$>
          c_is_valid_utf8 ptr (fromIntegral guessUtf8Boundary)
#else
      B.isValidUtf8 $ B.take guessUtf8Boundary (B.drop ofs bs)
#endif
    ) = slowValidateUtf8ChunkFrom (ofs + guessUtf8Boundary) bs
    -- No
  | otherwise = slowValidateUtf8ChunkFrom ofs bs
    where
      len = B.length bs - ofs
      isBoundary n word8 = len >= n && word8 <= B.index bs (ofs + len - n)
      guessUtf8Boundary
        | isBoundary 3 0xf0 = len - 3  -- third to last char starts a four-byte code point
        | isBoundary 2 0xe0 = len - 2  -- pre-last char starts a three-or-four-byte code point
        | isBoundary 1 0xc2 = len - 1  -- last char starts a two-(or more-)byte code point
        | otherwise = len
#else
  = slowValidateUtf8ChunkFrom ofs bs
#endif

-- | A pure Haskell implementation of validateUtf8More.
--
-- Ideally the primitives 'B.isValidUtf8' or 'c_is_valid_utf8' should give us
-- indices to let us avoid this function.
slowValidateUtf8ChunkFrom :: Int -> ByteString -> (Int, Maybe Utf8State)
slowValidateUtf8ChunkFrom ofs bs = loop ofs ofs utf8StartState
  where
    loop !utf8End i s
      | i < B.length bs =
          case updateUtf8State (B.index bs i) s of
            Just s' ->
              let utf8End' = if isUtf8StateIsComplete s' then i + 1 else utf8End
              in loop utf8End' (i + 1) s'
            Nothing -> (utf8End, Nothing)
      | otherwise = (utf8End, Just (Utf8State s (partUtf8UnsafeAppend partUtf8Empty (B.drop utf8End bs))))

-- | Validate a 'ByteString' as a contiuation of UTF-8-encoded text.
--
-- @validateNextUtf8Chunk chunk s = (n, es)@
--
-- This function returns two values:
--
-- * The value 'n' indicates the end position of longest prefix of the
--   'ByteString' that is valid UTF-8-encoded data from the starting
--   state 's'. If 's' contains an incomplete code point, the input
--   'ByteString' is considered a continuation. As a result 'n' will be
--   negative if the code point is still incomplete or is proven to be
--   invalid.
--   
-- * The value 'es' indicates whether the 'ByteString'
--
--     * (@Left p@) contains an invalid code point and where the next
--       (potentially valid) code point begins, so that @p - n@ is the
--       number of invalid bytes, or
--     * (@Right s'@) is valid, and all of the remaining bytes starting
--       at index 'n' are the beginning of an incomplete UTF-8 code
--       point, and `s'` is the resulting 'Utf8State' value, which
--       can be used to validate against a following 'ByteString'.
validateUtf8More :: Utf8State -> ByteString -> (Int, Maybe Utf8State)
validateUtf8More st@(Utf8State s0 part) bs
  | len > 0 = loop 0 s0
  | otherwise = (0, Just st)
  where
    len = B.length bs
    -- Complete an incomplete code point (if there is one)
    -- and then jump to validateUtf8ChunkFrom
    loop !i s
      | isUtf8StateIsComplete s = validateUtf8ChunkFrom i bs
      | i < len =
        case updateUtf8State (B.index bs i) s of
          Nothing -> (0, Nothing)
          Just s' -> loop (i + 1) s'
      | otherwise = (0, Just (Utf8State s (partUtf8UnsafeAppend part bs)))

-- | Construct an Array. This is currently an internal data structure which is
-- only used to construct Text (so for example byteStringToStrictBuilder should
-- only be applied to valid UTF-8 bytestrings).
data StrictBuilder = StrictBuilder
  { sbLength :: {-# UNPACK #-} !Int
  , sbWrite :: forall s. A.MArray s -> Int -> ST s ()
  }

emptyStrictBuilder :: StrictBuilder
emptyStrictBuilder = StrictBuilder 0 (\_ _ -> pure ())

-- | Right-biased append: run the right action first.  This allows a builder to
-- run tail-recursively when accumulating text left-to-right.
appendRStrictBuilder :: StrictBuilder -> StrictBuilder -> StrictBuilder
appendRStrictBuilder (StrictBuilder n1 write1) (StrictBuilder n2 write2) =
  StrictBuilder (n1 + n2) (\arr ofs -> do
    write2 arr (ofs + n1)
    write1 arr ofs)

copyFromByteString :: A.MArray s -> Int -> ByteString -> ST s ()
copyFromByteString dst ofs src = withBS src $ \ srcFPtr len ->
  unsafeIOToST $ unsafeWithForeignPtr srcFPtr $ \ srcPtr -> do
    unsafeSTToIO $ A.copyFromPointer dst ofs srcPtr len

byteStringToStrictBuilder :: ByteString -> StrictBuilder
byteStringToStrictBuilder bs =
  StrictBuilder (B.length bs) (\arr ofs -> copyFromByteString arr ofs bs)

charToStrictBuilder :: Char -> StrictBuilder
charToStrictBuilder c =
  StrictBuilder (utf8Length c) (\arr ofs -> void (Char.unsafeWrite arr ofs (safe c)))

word8ToStrictBuilder :: Word8 -> StrictBuilder
word8ToStrictBuilder w =
  StrictBuilder 1 (\arr ofs -> A.unsafeWrite arr ofs w)

partUtf8ToStrictBuilder :: PartialUtf8CodePoint -> StrictBuilder
partUtf8ToStrictBuilder =
  partUtf8Foldr (appendRStrictBuilder . word8ToStrictBuilder) emptyStrictBuilder

strictBuilderToText :: StrictBuilder -> Text
strictBuilderToText (StrictBuilder 0 _) = empty
strictBuilderToText (StrictBuilder n write) = runST (do
  dst <- A.new n
  write dst 0
  arr <- A.unsafeFreeze dst
  pure (Text arr 0 n))

-- | Decode a 'ByteString' in the context of what has been already been decoded.
--
-- The 'ByteString' is validated against the 'Utf8State' using the rules
-- governing 'validateNextUtf8Chunk'. The longest valid UTF-8 prefix is added
-- to the input 'TextDataStack' which is returned with the end position of the
-- valid prefix, and either the resulting 'Utf8State'
-- (@Right Utf8State@) or the position of the of the first (potentially)
-- valid byte after the invalid bytes with remainder of the 'ByteString'
-- (@Left (Int, ByteString)@).
decodeUtf8More :: Utf8State -> ByteString -> (StrictBuilder, ByteString, Maybe Utf8State)
decodeUtf8More s bs =
  case validateUtf8More s bs of
    (len, s') ->
      let builder | len == 0 = emptyStrictBuilder
                  | otherwise = partUtf8ToStrictBuilder (partialUtf8CodePoint s)
                    `appendRStrictBuilder` byteStringToStrictBuilder (B.take len bs)
      in (builder, B.drop len bs, s')

-- | Decode a 'ByteString' against a start 'Utf8State' with an empty
-- 'TextDataStack'.
--
-- @decodeUtf8More bs = 'decodeNextUtf8Chunk' bs 'startUtf8State' 'emptyStack'@
decodeUtf8Chunk :: ByteString -> (StrictBuilder, ByteString, Maybe Utf8State)
decodeUtf8Chunk = decodeUtf8More startUtf8State

-- | Call an error handler with the give 'String' message for each byte
-- in given 'ByteString' and lead data in the given 'Utf8State'
-- value. The bytes are the positions from 'errStart' (inclusive) to
-- 'errEnd' (exclusive). Any substitute characters are pushed onto the
-- supplied 'TextDataStack' argument.
skipIncomplete :: (Word8 -> Maybe Char) -> Utf8State -> StrictBuilder
skipIncomplete onErr s =
  partUtf8Foldr
    (appendRStrictBuilder . handleUtf8Error onErr)
    emptyStrictBuilder (partialUtf8CodePoint s)

handleUtf8Error :: (Word8 -> Maybe Char) -> Word8 -> StrictBuilder
handleUtf8Error onErr w = case onErr w of
  Just c -> charToStrictBuilder c
  Nothing -> emptyStrictBuilder

-- | Helper for 'decodeUtfWith'.
decodeUtf8With1 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  (Word8 -> Maybe Char) -> ByteString -> Text
decodeUtf8With1 onErr bs0 = strictBuilderToText $
    builder `appendRStrictBuilder` skipIncomplete onErr s
  where
    (builder, _, s) = decodeUtf8With2 onErr startUtf8State bs0

-- | Helper for 'decodeUtf8With' and 'streamDecodeUtf8With'.
-- This uses an 'OnDecodeError' to process bad bytes.
-- This is not a very pretty legacy API.
-- See 'decodeUtf8More' for a more flexible alternative.
decodeUtf8With2 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  (Word8 -> Maybe Char) -> Utf8State -> ByteString -> (StrictBuilder, ByteString, Utf8State)
decodeUtf8With2 onErr s0 bs = loop s0 0 emptyStrictBuilder
  where
    loop s i !builder =
      let nonEmptyPrefix len = builder
            `appendRStrictBuilder` partUtf8ToStrictBuilder (partialUtf8CodePoint s)
            `appendRStrictBuilder` byteStringToStrictBuilder (B.take len (B.drop i bs))
      in case validateUtf8More s (B.drop i bs) of
        (len, Nothing) ->
          if len == 0 && utf8CodePointState s /= utf8StartState
          then
            -- loop is strict on builder, so if onErr raises an error it will be forced here.
            let builder' = builder `appendRStrictBuilder` skipIncomplete onErr s
            in loop startUtf8State i builder'
          else
            let builder' = nonEmptyPrefix len
                  `appendRStrictBuilder` handleUtf8Error onErr (B.index bs (i + len))
            in loop startUtf8State (i + len + 1) builder'
        (len, Just s') ->
          let builder' = if len == 0 then builder else nonEmptyPrefix len
              undecoded = if B.length bs - i - len == partUtf8Len (partialUtf8CodePoint s')
                then B.drop (i + len) bs  -- Reuse bs if possible
                else partUtf8ToByteString (partialUtf8CodePoint s')
          in (builder', undecoded, s')
