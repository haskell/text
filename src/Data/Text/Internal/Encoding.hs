{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Internals of "Data.Text.Encoding".
--
-- If you'd like to depend on something from here that's not in "Data.Text.Encoding",
-- please request to have it exported!
module Data.Text.Internal.Encoding where

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.Bits ((.&.), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Semigroup (Semigroup(..))
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
-- Enables stream decoding ('validateUtf8Chunk', 'validateUtf8More',
-- 'decodeUtf8Chunk', 'decodeUtf8More').

-- Internal invariant:
-- the first component is the initial state if and only if
-- the second component is empty.
--
-- @
-- 'utf9CodePointState' s = 'utf8StartState'
-- <=>
-- 'partialUtf8CodePoint' s = 'PartialUtf8CodePoint' 0
-- @
--
-- @since 2.0.2
data Utf8State = Utf8State
  { -- | State of the UTF-8 state machine.
    utf8CodePointState :: {-# UNPACK #-} !Utf8CodePointState
    -- | Bytes of the currently incomplete code point (if any).
  , partialUtf8CodePoint :: {-# UNPACK #-} !PartialUtf8CodePoint
  }
  deriving (Eq, Ord, Show)

-- | Initial 'Utf8State'.
--
-- @since 2.0.2
startUtf8State :: Utf8State
startUtf8State = Utf8State utf8StartState partUtf8Empty

-- | Prefix of a UTF-8 code point encoded in 4 bytes,
-- possibly empty.
--
-- - The most significant byte contains the number of bytes,
--   between 0 and 3.
-- - The remaining bytes hold the incomplete code point.
-- - Unused bytes must be 0.
--
-- All of operations available on it are the functions below.
-- The constructor should never be used outside of those.
--
-- @since 2.0.2
newtype PartialUtf8CodePoint = PartialUtf8CodePoint Word32
  deriving (Eq, Ord, Show)

-- | Empty prefix.
partUtf8Empty :: PartialUtf8CodePoint
partUtf8Empty = PartialUtf8CodePoint 0

-- | Length of the partial code point, stored in the most significant byte.
partUtf8Len :: PartialUtf8CodePoint -> Int
partUtf8Len (PartialUtf8CodePoint w) = fromIntegral $ w `shiftR` 24

-- | Length of the code point once completed (it is known in the first byte).
-- 0 if empty.
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
--
-- Unsafe: no bounds checking.
partUtf8UnsafeIndex ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  PartialUtf8CodePoint -> Int -> Word8
partUtf8UnsafeIndex _c@(PartialUtf8CodePoint w) n =
#if defined(ASSERTS)
  assert (0 <= n && n < partUtf8Len _c) $
#endif
  fromIntegral $ w `shiftR` (16 - 8 * n)

-- | Append some bytes.
--
-- Unsafe: no bounds checking.
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

-- | Fold a 'PartialUtf8CodePoint'. This avoids recursion so it unfolds to straightline code.
partUtf8Foldr :: (Word8 -> a -> a) -> a -> PartialUtf8CodePoint -> a
partUtf8Foldr f x0 c = case partUtf8Len c of
    0 -> x0
    1 -> build 0 x0
    2 -> build 0 (build 1 x0)
    _ -> build 0 (build 1 (build 2 x0))
  where
    build i x = f (partUtf8UnsafeIndex c i) x

-- | Convert 'PartialUtf8CodePoint' to 'ByteString'.
partUtf8ToByteString :: PartialUtf8CodePoint -> B.ByteString
partUtf8ToByteString c = BI.unsafeCreate (partUtf8Len c) $ \ptr ->
  partUtf8Foldr (\w k i -> pokeElemOff ptr i w >> k (i+1)) (\_ -> pure ()) c 0

#ifdef SIMDUTF
foreign import ccall unsafe "_hs_text_is_valid_utf8" c_is_valid_utf8
    :: Ptr Word8 -> CSize -> IO CInt
#endif

-- | Validate a 'ByteString' as UTF-8-encoded text. To be continued using 'validateUtf8More'.
--
-- See also 'validateUtf8More' for details on the result of this function.
--
-- @
-- 'validateUtf8Chunk' = 'validateUtf8More' 'startUtf8State'
-- @
--
-- === Properties
--
-- Given:
--
-- @
-- 'validateUtf8Chunk' chunk = (n, ms)
-- @
--
-- - The prefix is valid UTF-8. In particular, it should be accepted
--   by this validation:
--
--   @
--   'validateUtf8Chunk' ('Data.ByteString.take' n chunk) = (n, Just 'startUtf8State')
--   @
--
-- @since 2.0.2
validateUtf8Chunk :: ByteString -> (Int, Maybe Utf8State)
validateUtf8Chunk = validateUtf8ChunkFrom 0

-- | Add an offset to the index returned by 'validateUtf8More'.
--
-- @
-- validateUtf8ChunkFrom n = first (+ n) . 'validateUtf8More' . 'B.drop' n
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

-- | Validate another 'ByteString' chunk in an ongoing stream of UTF-8-encoded text.
--
-- Returns a pair:
--
-- 1. The first component @n@ is the end position, relative to the current
--    chunk, of the longest prefix of the accumulated bytestring which is valid UTF-8.
--    @n@ may be negative: that happens when an incomplete code point started in
--    a previous chunk and is not completed by the current chunk (either
--    that code point is still incomplete, or it is broken by an invalid byte).
--
-- 2. The second component @ms@ indicates the following:
--
--     - if @ms = Nothing@, the remainder of the chunk contains an invalid byte,
--       within four bytes from position @n@;
--     - if @ms = Just s'@, you can carry on validating another chunk
--       by calling 'validateUtf8More' with the new state @s'@.
--
-- === Properties
--
-- Given:
--
-- @
-- 'validateUtf8More' s chunk = (n, ms)
-- @
--
-- - If the chunk is invalid, it cannot be extended to be valid.
--
--   @
--   ms = Nothing
--   ==> 'validateUtf8More' s (chunk '<>' more) = (n, Nothing)
--   @
--
-- - Validating two chunks sequentially is the same as validating them
--   together at once:
--
--   @
--   ms = Just s'
--   ==> 'validateUtf8More' s (chunk '<>' more) = 'Data.Bifunctor.first' ('Data.ByteString.length' chunk '+') ('validateUtf8More' s' more)
--   @
--
-- @since 2.0.2
validateUtf8More :: Utf8State -> ByteString -> (Int, Maybe Utf8State)
validateUtf8More st@(Utf8State s0 part) bs
  | len > 0 = loop 0 s0
  | otherwise = (- partUtf8Len part, Just st)
  where
    len = B.length bs
    -- Complete an incomplete code point (if there is one)
    -- and then jump to validateUtf8ChunkFrom
    loop !i s
      | isUtf8StateIsComplete s = validateUtf8ChunkFrom i bs
      | i < len =
        case updateUtf8State (B.index bs i) s of
          Nothing -> (- partUtf8Len part, Nothing)
          Just s' -> loop (i + 1) s'
      | otherwise = (- partUtf8Len part, Just (Utf8State s (partUtf8UnsafeAppend part bs)))

-- | A delayed representation of strict 'Text'.

-- For internal purposes, this is instead used as a delayed 'Array':
-- it may not actually be valid 'Text' (e.g., 'word8ToStrictBuilder',
-- 'byteStringToStrictBuilder', 'partUtf8ToStrictBuilder').
--
-- @since 2.0.2
data StrictBuilder = StrictBuilder
  { sbLength :: {-# UNPACK #-} !Int
  , sbWrite :: forall s. A.MArray s -> Int -> ST s ()
  }

-- | Concatenation of 'StrictBuilder' is right-biased:
-- the right builder will be run first. This allows a builder to
-- run tail-recursively when it was accumulated left-to-right.
instance Semigroup StrictBuilder where
  (<>) = appendRStrictBuilder

instance Monoid StrictBuilder where
  mempty = emptyStrictBuilder
  mappend = (<>)

emptyStrictBuilder :: StrictBuilder
emptyStrictBuilder = StrictBuilder 0 (\_ _ -> pure ())

appendRStrictBuilder :: StrictBuilder -> StrictBuilder -> StrictBuilder
appendRStrictBuilder (StrictBuilder n1 write1) (StrictBuilder n2 write2) =
  StrictBuilder (n1 + n2) (\dst ofs -> do
    write2 dst (ofs + n1)
    write1 dst ofs)

copyFromByteString :: A.MArray s -> Int -> ByteString -> ST s ()
copyFromByteString dst ofs src = withBS src $ \ srcFPtr len ->
  unsafeIOToST $ unsafeWithForeignPtr srcFPtr $ \ srcPtr -> do
    unsafeSTToIO $ A.copyFromPointer dst ofs srcPtr len

byteStringToStrictBuilder :: ByteString -> StrictBuilder
byteStringToStrictBuilder bs =
  StrictBuilder (B.length bs) (\dst ofs -> copyFromByteString dst ofs bs)

charToStrictBuilder :: Char -> StrictBuilder
charToStrictBuilder c =
  StrictBuilder (utf8Length c) (\dst ofs -> void (Char.unsafeWrite dst ofs (safe c)))

word8ToStrictBuilder :: Word8 -> StrictBuilder
word8ToStrictBuilder w =
  StrictBuilder 1 (\dst ofs -> A.unsafeWrite dst ofs w)

partUtf8ToStrictBuilder :: PartialUtf8CodePoint -> StrictBuilder
partUtf8ToStrictBuilder =
  partUtf8Foldr ((<>) . word8ToStrictBuilder) emptyStrictBuilder

-- | Use 'StrictBuilder' to build 'Text'.
--
-- @since 2.0.2
strictBuilderToText :: StrictBuilder -> Text
strictBuilderToText (StrictBuilder 0 _) = empty
strictBuilderToText (StrictBuilder n write) = runST (do
  dst <- A.new n
  write dst 0
  arr <- A.unsafeFreeze dst
  pure (Text arr 0 n))

-- | Copy 'Text' in a 'StrictBuilder'
--
-- @since 2.0.2
textToStrictBuilder :: Text -> StrictBuilder
textToStrictBuilder (Text src srcOfs n) = StrictBuilder n (\dst dstOfs ->
  A.copyI n dst dstOfs src srcOfs)

-- | Decode another chunk in an ongoing UTF-8 stream.
--
-- Returns a triple:
--
-- 1. A 'StrictBuilder' for the decoded chunk of text. You can accumulate
--    chunks with @('<>')@ or output them with 'strictBuilderToText'.
-- 2. The undecoded remainder of the given chunk, for diagnosing errors
--    and resuming (presumably after skipping some bytes).
-- 3. 'Just' the new state, or 'Nothing' if an invalid byte was encountered
--    (it will be within the first 4 bytes of the undecoded remainder).
--
-- @since 2.0.2
decodeUtf8More :: Utf8State -> ByteString -> (StrictBuilder, ByteString, Maybe Utf8State)
decodeUtf8More s bs =
  case validateUtf8More s bs of
    (len, s') ->
      let builder | len <= 0 = emptyStrictBuilder
                  | otherwise = partUtf8ToStrictBuilder (partialUtf8CodePoint s)
                    <> byteStringToStrictBuilder (B.take len bs)
      in (builder, B.drop len bs, s')

-- | Decode a chunk of UTF-8 text. To be continued with 'decodeUtf8More'.
--
-- See 'decodeUtf8More' for details on the result.
--
-- === Properties
--
-- @
-- 'decodeUtf8Chunk' = 'decodeUtf8More' 'startUtf8State'
-- @
--
-- Given:
--
-- @
-- 'decodeUtf8Chunk' chunk = (builder, rest, ms)
-- @
--
-- @builder@ is a prefix and @rest@ is a suffix of @chunk@.
--
-- @
-- 'Data.Text.Encoding.encodeUtf8' ('strictBuilderToText' builder) '<>' rest = chunk
-- @
--
-- @since 2.0.2
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
    ((<>) . handleUtf8Error onErr)
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
    builder <> skipIncomplete onErr s
  where
    (builder, _, s) = decodeUtf8With2 onErr startUtf8State bs0

-- | Helper for 'Data.Text.Encoding.decodeUtf8With',
-- 'Data.Text.Encoding.streamDecodeUtf8With', and lazy
-- 'Data.Text.Lazy.Encoding.decodeUtf8With',
-- which use an 'OnDecodeError' to process bad bytes.
--
-- See 'decodeUtf8Chunk' for a more flexible alternative.
decodeUtf8With2 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  (Word8 -> Maybe Char) -> Utf8State -> ByteString -> (StrictBuilder, ByteString, Utf8State)
decodeUtf8With2 onErr s0 bs = loop s0 0 emptyStrictBuilder
  where
    loop s i !builder =
      let nonEmptyPrefix len = builder
            <> partUtf8ToStrictBuilder (partialUtf8CodePoint s)
            <> byteStringToStrictBuilder (B.take len (B.drop i bs))
      in case validateUtf8More s (B.drop i bs) of
        (len, Nothing) ->
          if len < 0
          then
            -- If the first byte cannot complete the partial code point in s,
            -- retry from startUtf8State.
            let builder' = builder <> skipIncomplete onErr s
            -- Note: loop is strict on builder, so if onErr raises an error it will
            -- be forced here, short-circuiting the loop as desired.
            in loop startUtf8State i builder'
          else
            let builder' = nonEmptyPrefix len
                  <> handleUtf8Error onErr (B.index bs (i + len))
            in loop startUtf8State (i + len + 1) builder'
        (len, Just s') ->
          let builder' = if len <= 0 then builder else nonEmptyPrefix len
              undecoded = if B.length bs >= partUtf8Len (partialUtf8CodePoint s')
                then B.drop (i + len) bs  -- Reuse bs if possible
                else partUtf8ToByteString (partialUtf8CodePoint s')
          in (builder', undecoded, s')
