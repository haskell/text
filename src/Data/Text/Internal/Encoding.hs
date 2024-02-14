{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Data.Text.Internal.Builder
-- License     : BSD-style (see LICENSE)
-- Stability   : experimental
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Internals of "Data.Text.Encoding".
--
-- @since 2.0.2
module Data.Text.Internal.Encoding
  ( validateUtf8Chunk
  , validateUtf8More
  , decodeUtf8Chunk
  , decodeUtf8More
  , decodeUtf8With1
  , decodeUtf8With2
  , Utf8State
  , startUtf8State
  , StrictBuilder()
  , strictBuilderToText
  , textToStrictBuilder

    -- * Internal
  , skipIncomplete
  , getCompleteLen
  , getPartialUtf8
  ) where

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.Bits ((.&.), shiftL, shiftR)
import Data.ByteString (ByteString)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word (Word32, Word8)
import Foreign.Storable (pokeElemOff)
import Data.Text.Encoding.Error (OnDecodeError)
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Encoding.Utf8
  (DecoderState, utf8AcceptState, utf8RejectState, updateDecoderState)
import Data.Text.Internal.StrictBuilder (StrictBuilder)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.StrictBuilder as SB
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

#ifdef SIMDUTF
import Data.Text.Internal.ByteStringCompat (withBS)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
#endif

-- | Use 'StrictBuilder' to build 'Text'.
--
-- @since 2.0.2
strictBuilderToText :: StrictBuilder -> Text
strictBuilderToText = SB.toText

-- | Copy 'Text' in a 'StrictBuilder'
--
-- @since 2.0.2
textToStrictBuilder :: Text -> StrictBuilder
textToStrictBuilder = SB.fromText

-- | State of decoding a 'ByteString' in UTF-8.
-- Enables incremental decoding ('validateUtf8Chunk', 'validateUtf8More',
-- 'decodeUtf8Chunk', 'decodeUtf8More').
--
-- @since 2.0.2

-- Internal invariant:
-- the first component is the initial state if and only if
-- the second component is empty.
--
-- @
-- 'utf9CodePointState' s = 'utf8StartState'
-- <=>
-- 'partialUtf8CodePoint' s = 'PartialUtf8CodePoint' 0
-- @
data Utf8State = Utf8State
  { -- | State of the UTF-8 state machine.
    utf8CodePointState :: {-# UNPACK #-} !DecoderState
    -- | Bytes of the currently incomplete code point (if any).
  , partialUtf8CodePoint :: {-# UNPACK #-} !PartialUtf8CodePoint
  }
  deriving (Eq, Show)

-- | Initial 'Utf8State'.
--
-- @since 2.0.2
startUtf8State :: Utf8State
startUtf8State = Utf8State utf8AcceptState partUtf8Empty

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
  deriving (Eq, Show)

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

-- | Fold a 'PartialUtf8CodePoint'. This avoids recursion so it can unfold to straightline code.
{-# INLINE partUtf8Foldr #-}
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

-- | Exported for testing.
getCompleteLen :: Utf8State -> Int
getCompleteLen = partUtf8CompleteLen . partialUtf8CodePoint

-- | Exported for testing.
getPartialUtf8 :: Utf8State -> B.ByteString
getPartialUtf8 = partUtf8ToByteString . partialUtf8CodePoint

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
-- @since 2.0.2
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
--     @
--     'validateUtf8Chunk' ('Data.ByteString.take' n chunk) = (n, Just 'startUtf8State')
--     @
validateUtf8Chunk :: ByteString -> (Int, Maybe Utf8State)
validateUtf8Chunk bs = validateUtf8ChunkFrom 0 bs (,)

-- Assume bytes up to offset @ofs@ have been validated already.
--
-- Using CPS lets us inline the continuation and avoid allocating a @Maybe@
-- in the @decode...@ functions.
{-# INLINE validateUtf8ChunkFrom #-}
validateUtf8ChunkFrom :: forall r. Int -> ByteString -> (Int -> Maybe Utf8State -> r) -> r
validateUtf8ChunkFrom ofs bs k
  -- B.isValidUtf8 is buggy before bytestring-0.11.5.3 / bytestring-0.12.1.0.
  -- MIN_VERSION_bytestring does not allow us to differentiate
  -- between 0.11.5.2 and 0.11.5.3 so no choice except demanding 0.12.1+.
#if defined(SIMDUTF) || MIN_VERSION_bytestring(0,12,1)
  | guessUtf8Boundary > 0 &&
    -- the rest of the bytestring is valid utf-8 up to the boundary
    (
#ifdef SIMDUTF
      withBS (B.drop ofs bs) $ \ fp _ -> unsafeDupablePerformIO $
        unsafeWithForeignPtr fp $ \ptr -> (/= 0) <$>
          c_is_valid_utf8 ptr (fromIntegral guessUtf8Boundary)
#else
      B.isValidUtf8 $ B.take guessUtf8Boundary (B.drop ofs bs)
#endif
    ) = slowValidateUtf8ChunkFrom (ofs + guessUtf8Boundary)
    -- No
  | otherwise = slowValidateUtf8ChunkFrom ofs
  where
    len = B.length bs - ofs
    isBoundary n p = len >= n && p (B.index bs (ofs + len - n))
    guessUtf8Boundary
      | isBoundary 1 (<= 0x80) = len      -- last char is ASCII (common short-circuit)
      | isBoundary 1 (0xc2 <=) = len - 1  -- last char starts a two-(or more-)byte code point
      | isBoundary 2 (0xe0 <=) = len - 2  -- pre-last char starts a three-or-four-byte code point
      | isBoundary 3 (0xf0 <=) = len - 3  -- third to last char starts a four-byte code point
      | otherwise = len
#else
  = slowValidateUtf8ChunkFrom ofs
  where
#endif
    -- A pure Haskell implementation of validateUtf8More.
    -- Ideally the primitives 'B.isValidUtf8' or 'c_is_valid_utf8' should give us
    -- indices to let us avoid this function.
    slowValidateUtf8ChunkFrom :: Int -> r
    slowValidateUtf8ChunkFrom ofs1 = slowLoop ofs1 ofs1 utf8AcceptState

    slowLoop !utf8End i s
      | i < B.length bs =
          case updateDecoderState (B.index bs i) s of
            s' | s' == utf8RejectState -> k utf8End Nothing
               | s' == utf8AcceptState -> slowLoop (i + 1) (i + 1) s'
               | otherwise -> slowLoop utf8End (i + 1) s'
      | otherwise = k utf8End (Just (Utf8State s (partUtf8UnsafeAppend partUtf8Empty (B.drop utf8End bs))))

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
-- @since 2.0.2
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
--     @
--     ms = Nothing
--     ==> 'validateUtf8More' s (chunk '<>' more) = (n, Nothing)
--     @
--
-- - Validating two chunks sequentially is the same as validating them
--   together at once:
--
--     @
--     ms = Just s'
--     ==> 'validateUtf8More' s (chunk '<>' more) = 'Data.Bifunctor.first' ('Data.ByteString.length' chunk '+') ('validateUtf8More' s' more)
--     @
validateUtf8More :: Utf8State -> ByteString -> (Int, Maybe Utf8State)
validateUtf8More st bs = validateUtf8MoreCont st bs (,)

-- CPS: inlining the continuation lets us make more tail calls and avoid
-- allocating a @Maybe@ in @decodeWith1/2@.
{-# INLINE validateUtf8MoreCont #-}
validateUtf8MoreCont :: Utf8State -> ByteString -> (Int -> Maybe Utf8State -> r) -> r
validateUtf8MoreCont st@(Utf8State s0 part) bs k
  | len > 0 = loop 0 s0
  | otherwise = k (- partUtf8Len part) (Just st)
  where
    len = B.length bs
    -- Complete an incomplete code point (if there is one)
    -- and then jump to validateUtf8ChunkFrom
    loop !i s
      | s == utf8AcceptState = validateUtf8ChunkFrom i bs k
      | i < len =
        case updateDecoderState (B.index bs i) s of
          s' | s' == utf8RejectState -> k (- partUtf8Len part) Nothing
             | otherwise -> loop (i + 1) s'
      | otherwise = k (- partUtf8Len part) (Just (Utf8State s (partUtf8UnsafeAppend part bs)))

-- Eta-expanded to inline partUtf8Foldr
partUtf8ToStrictBuilder :: PartialUtf8CodePoint -> StrictBuilder
partUtf8ToStrictBuilder c =
  partUtf8Foldr ((<>) . SB.unsafeFromWord8) mempty c

utf8StateToStrictBuilder :: Utf8State -> StrictBuilder
utf8StateToStrictBuilder = partUtf8ToStrictBuilder . partialUtf8CodePoint

-- | Decode another chunk in an ongoing UTF-8 stream.
--
-- Returns a triple:
--
-- 1. A 'StrictBuilder' for the decoded chunk of text. You can accumulate
--    chunks with @('<>')@ or output them with 'SB.toText'.
-- 2. The undecoded remainder of the given chunk, for diagnosing errors
--    and resuming (presumably after skipping some bytes).
-- 3. 'Just' the new state, or 'Nothing' if an invalid byte was encountered
--    (it will be within the first 4 bytes of the undecoded remainder).
--
-- @since 2.0.2
--
-- === Properties
--
-- Given:
--
-- @
-- (pre, suf, ms) = 'decodeUtf8More' s chunk
-- @
--
-- 1. If the output @pre@ is nonempty (alternatively, if @length chunk > length suf@)
--
--     @
--     s2b pre \`'Data.ByteString.append'\` suf = p2b s \`'Data.ByteString.append'\` chunk
--     @
--
--     where
--
--     @
--     s2b = 'Data.Text.Encoding.encodeUtf8' . 'Data.Text.Encoding.toText'
--     p2b = 'Data.Text.Internal.Encoding.partUtf8ToByteString'
--     @
--
-- 2. If the output @pre@ is empty (alternatively, if @length chunk = length suf@)
--
--     @suf = chunk@
--
-- 3. Decoding chunks separately is equivalent to decoding their concatenation.
--
--     Given:
--
--     @
--     (pre1, suf1, Just s1) = 'decodeUtf8More' s chunk1
--     (pre2, suf2,     ms2) = 'decodeUtf8More' s1 chunk2
--     (pre3, suf3,     ms3) = 'decodeUtf8More' s (chunk1 \`B.append\` chunk2)
--     @
--
--     we have:
--
--     @
--     s2b (pre1 '<>' pre2) = s2b pre3
--     ms2 = ms3
--     @
decodeUtf8More :: Utf8State -> ByteString -> (StrictBuilder, ByteString, Maybe Utf8State)
decodeUtf8More s bs =
  validateUtf8MoreCont s bs $ \len ms ->
    let builder | len <= 0 = mempty
                | otherwise = utf8StateToStrictBuilder s
                  <> SB.unsafeFromByteString (B.take len bs)
    in (builder, B.drop len bs, ms)

-- | Decode a chunk of UTF-8 text. To be continued with 'decodeUtf8More'.
--
-- See 'decodeUtf8More' for details on the result.
--
-- @since 2.0.2
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
-- 'Data.Text.Encoding.encodeUtf8' ('Data.Text.Encoding.strictBuilderToText' builder) '<>' rest = chunk
-- @
decodeUtf8Chunk :: ByteString -> (StrictBuilder, ByteString, Maybe Utf8State)
decodeUtf8Chunk = decodeUtf8More startUtf8State

-- | Call the error handler on each byte of the partial code point stored in
-- 'Utf8State' and append the results.
--
-- Exported for use in lazy 'Data.Text.Lazy.Encoding.decodeUtf8With'.
--
-- @since 2.0.2
{-# INLINE skipIncomplete #-}
skipIncomplete :: OnDecodeError -> String -> Utf8State -> StrictBuilder
skipIncomplete onErr msg s =
  partUtf8Foldr
    ((<>) . handleUtf8Error onErr msg)
    mempty (partialUtf8CodePoint s)

{-# INLINE handleUtf8Error #-}
handleUtf8Error :: OnDecodeError -> String -> Word8 -> StrictBuilder
handleUtf8Error onErr msg w = case onErr msg (Just w) of
  Just c -> SB.fromChar c
  Nothing -> mempty

-- | Helper for 'Data.Text.Encoding.decodeUtf8With'.
--
-- @since 2.0.2

-- This could be shorter by calling 'decodeUtf8With2' directly, but we make the
-- first call validateUtf8Chunk directly to return even faster in successful
-- cases.
decodeUtf8With1 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> String -> ByteString -> Text
decodeUtf8With1 onErr msg bs = validateUtf8ChunkFrom 0 bs $ \len ms -> case ms of
    Just s
      | len == B.length bs ->
        let !(SBS.SBS arr) = SBS.toShort bs in
        Text (A.ByteArray arr) 0 len
      | otherwise -> SB.toText $
          SB.unsafeFromByteString (B.take len bs) <> skipIncomplete onErr msg s
    Nothing ->
       let (builder, _, s) = decodeUtf8With2 onErr msg startUtf8State (B.drop (len + 1) bs) in
       SB.toText $
         SB.unsafeFromByteString (B.take len bs) <>
         handleUtf8Error onErr msg (B.index bs len) <>
         builder <>
         skipIncomplete onErr msg s

-- | Helper for 'Data.Text.Encoding.decodeUtf8With',
-- 'Data.Text.Encoding.streamDecodeUtf8With', and lazy
-- 'Data.Text.Lazy.Encoding.decodeUtf8With',
-- which use an 'OnDecodeError' to process bad bytes.
--
-- See 'decodeUtf8Chunk' for a more flexible alternative.
--
-- @since 2.0.2
decodeUtf8With2 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> String -> Utf8State -> ByteString -> (StrictBuilder, ByteString, Utf8State)
decodeUtf8With2 onErr msg s0 bs = loop s0 0 mempty
  where
    loop s i !builder =
      let nonEmptyPrefix len = builder
            <> utf8StateToStrictBuilder s
            <> SB.unsafeFromByteString (B.take len (B.drop i bs))
      in validateUtf8MoreCont s (B.drop i bs) $ \len ms -> case ms of
        Nothing ->
          if len < 0
          then
            -- If the first byte cannot complete the partial code point in s,
            -- retry from startUtf8State.
            let builder' = builder <> skipIncomplete onErr msg s
            -- Note: loop is strict on builder, so if onErr raises an error it will
            -- be forced here, short-circuiting the loop as desired.
            in loop startUtf8State i builder'
          else
            let builder' = nonEmptyPrefix len
                  <> handleUtf8Error onErr msg (B.index bs (i + len))
            in loop startUtf8State (i + len + 1) builder'
        Just s' ->
          let builder' = if len <= 0 then builder else nonEmptyPrefix len
              undecoded = if B.length bs >= partUtf8Len (partialUtf8CodePoint s')
                then B.drop (i + len) bs  -- Reuse bs if possible
                else partUtf8ToByteString (partialUtf8CodePoint s')
          in (builder', undecoded, s')
