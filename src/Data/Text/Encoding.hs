{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module      : Data.Text.Encoding
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts,
--               (c) 2008, 2009 Tom Harper
--               (c) 2021 Andrew Lelechenko
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : portable
--
-- Functions for converting 'Text' values to and from 'ByteString',
-- using several standard encodings.
--
-- To gain access to a much larger family of encodings, use the
-- <http://hackage.haskell.org/package/text-icu text-icu package>.

module Data.Text.Encoding
    (
    -- * Decoding ByteStrings to Text
    -- $strict

    -- ** Total Functions #total#
    -- $total
      decodeLatin1
    , decodeUtf8Lenient
    , DecodeResult(..)
    , decodeAsciiChunks
    , decodeUtf8Chunks
    , decodeUtf16Chunks
    , decodeUtf32Chunks

    -- *** Catchable failure
    , decodeUtf8'

    -- *** Controllable error handling
    , decodeUtf8With
    , decodeUtf16LEWith
    , decodeUtf16BEWith
    , decodeUtf32LEWith
    , decodeUtf32BEWith

    -- *** Stream oriented decoding
    -- $stream
    , streamDecodeUtf8
    , streamDecodeUtf8With
    , Decoding(..)

    -- ** Partial Functions
    -- $partial
    , decodeASCII
    , decodeUtf8
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE

    -- * Encoding Text to ByteStrings
    , encodeUtf8
    , encodeUtf16LE
    , encodeUtf16BE
    , encodeUtf32LE
    , encodeUtf32BE

    -- * Encoding Text using ByteString Builders
    , encodeUtf8Builder
    , encodeUtf8BuilderEscaped
    ) where

import Control.Exception (evaluate, try)
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.Bits (Bits, shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Text.Encoding.Common (DecodeResult(..), OnDecodeError, UnicodeException, strictDecode, lenientDecode)
import Data.Text.Internal (Text(..), empty, append)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Data.Text.Internal.Unsafe.Char (unsafeWrite)
import Data.Text.Show as T (singleton)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word8, Word16, Word32)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (Storable(..), poke, peekByteOff)
import GHC.Exts (byteArrayContents#, unsafeCoerce#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(PlainPtr))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Internal as B hiding (empty, append)
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder.Prim.Internal as BP
import Data.Text.Internal.Encoding.Utf8 (utf8DecodeStart, utf8DecodeContinue, DecoderResult(..))
import Data.Text.Internal.Encoding.Utf16 (Utf16Result(..), queryUtf16Bytes)
import Data.Text.Internal.Encoding.Utf32 (queryUtf32Bytes)
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Encoding.Fusion as E
import qualified Data.Text.Internal.Fusion as F
import Data.Text.Internal.ByteStringCompat
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

#ifdef SIMDUTF
import Foreign.C.Types (CInt(..))
#elif !MIN_VERSION_bytestring(0,11,2)
import qualified Data.ByteString.Unsafe as B
import Data.Text.Internal.Encoding.Utf8 (CodePoint(..))
#endif

-- $strict
--
-- All of the single-parameter functions for decoding bytestrings
-- encoded in one of the Unicode Transformation Formats (UTF) operate
-- in a /strict/ mode: each will throw an exception if given invalid
-- input.
--
-- Each function has a variant, whose name is suffixed with -'With',
-- that gives greater control over the handling of decoding errors.
-- For instance, 'decodeUtf8' will throw an exception, but
-- 'decodeUtf8With' allows the programmer to determine what to do on a
-- decoding error.

-- $total
--
-- These functions facilitate total decoding and should be preferred
-- over their partial counterparts.

-- $partial
--
-- These functions are partial and should only be used with great caution
-- (preferably not at all). See "Data.Text.Encoding#g:total" for better
-- solutions.

-- | Decode a 'ByteString' containing 7-bit ASCII
-- encoded text.
--
-- This is a partial function: it checks that input does not contain
-- anything except ASCII and copies buffer or throws an error otherwise.
--
decodeASCII :: ByteString -> Text
decodeASCII bs = case decodeAsciiChunks bs mempty of
  DecodeResult t mW _ pos ->
    case mW of
      Just _ -> error $ "decodeASCII: detected non-ASCII codepoint at " ++ show (pos - 1)
      _ -> t

-- | Decode a 'ByteString' containing Latin-1 (aka ISO-8859-1) encoded text.
--
-- 'decodeLatin1' is semantically equivalent to
--  @Data.Text.pack . Data.ByteString.Char8.unpack@
--
-- This is a total function. However, bear in mind that decoding Latin-1 (non-ASCII)
-- characters to UTf-8 requires actual work and is not just buffer copying.
--
decodeLatin1 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Text
decodeLatin1 bs = withBS bs $ \fp len -> runST $ do
  dst <- A.new (2 * len)
  let inner srcOff dstOff = if srcOff >= len then return dstOff else do
        asciiPrefixLen <- fmap fromIntegral . unsafeIOToST . unsafeWithForeignPtr fp $ \src ->
          c_is_ascii (src `plusPtr` srcOff) (src `plusPtr` len)
        if asciiPrefixLen == 0
        then do
          byte <- unsafeIOToST $ unsafeWithForeignPtr fp $ \src -> peekByteOff src srcOff
          A.unsafeWrite dst dstOff (0xC0 + (byte `shiftR` 6))
          A.unsafeWrite dst (dstOff + 1) (0x80 + (byte .&. 0x3F))
          inner (srcOff + 1) (dstOff + 2)
        else do
          unsafeIOToST $ unsafeWithForeignPtr fp $ \src ->
            unsafeSTToIO $ A.copyFromPointer dst dstOff (src `plusPtr` srcOff) asciiPrefixLen
          inner (srcOff + asciiPrefixLen) (dstOff + asciiPrefixLen)

  actualLen <- inner 0 0
  dst' <- A.resizeM dst actualLen
  arr <- A.unsafeFreeze dst'
  return $ Text arr 0 actualLen

foreign import ccall unsafe "_hs_text_is_ascii" c_is_ascii
    :: Ptr Word8 -> Ptr Word8 -> IO CSize

data Progression
  = WriteAndAdvance Char Int
  | NeedMore
  | Invalid

decodeChunks ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  (Bits w, Num w, Storable w)
  => w
  -> ( Int
    -> ByteString
    -> A.MArray s
    -> Int
    -> Int
    -> Int
    -> ST s (Maybe (
        (A.MArray s -> Int -> Int -> Int -> ST s (DecodeResult Text ByteString w))
        -> ST s (DecodeResult Text ByteString w))
      )
    )
  -> ((Int -> Word8) -> Int -> Int -> Progression)
  -> ByteString
  -> ByteString
  -> ST s (DecodeResult Text ByteString w)
decodeChunks w bulkDecodeF incrementalDecodeF bs1@(B.length -> len1) bs2@(B.length -> len2) = do
  marr <- A.new len'
  outer marr len' 0 0
  where
    wordByteSize = sizeOf w

    index :: Int -> Word8
    index i
      | i < len1  = B.index bs1 i
      | otherwise = B.index bs2 $ i - len1

    len :: Int
    len = len1 + len2
    len' :: Int
    len' = (len `div` wordByteSize) + 4

    outer dst dstLen = inner
      where
        inner srcOff dstOff
          | srcOff < len1 = decode 0 bs1
          | srcOff < len = decode len1 bs2
          -- finished (for now)
          | otherwise = goodSoFar
          where
            decode bsOff bs = do
              mOuterArgs <- bulkDecodeF bsOff bs dst dstLen srcOff dstOff
              case mOuterArgs of
                Just outerArgs -> outerArgs outer
                _ -> if len - srcOff < wordByteSize
                  -- incomplete code point
                  then goodSoFar
                  else
                    if dstOff + 4 > dstLen
                      -- need more space in destination
                      then do
                        let dstLen' = dstLen + 4
                        dst' <- A.resizeM dst dstLen'
                        outer dst' dstLen' srcOff dstOff
                      else
                        case incrementalDecodeF index len srcOff of
                          WriteAndAdvance c srcOff' -> do
                            d <- unsafeWrite dst dstOff c
                            inner srcOff' $ dstOff + d
                          NeedMore -> goodSoFar
                          Invalid -> invalid
            contin off res = do
              A.shrinkM dst dstOff
              arr <- A.unsafeFreeze dst
              pure . res (Text arr 0 dstOff) $ if off >= len1
                then B.drop (off - len1) bs2
                else B.drop off $ bs1 `B.append` bs2
            goodSoFar =
              contin srcOff $ \ t bs' ->
                DecodeResult t Nothing bs' srcOff
            invalid =
              let srcOff' = srcOff + wordByteSize
                  bytesToWord n word
                    | n > 0 = bytesToWord (n - 1) $ (fromIntegral . index $ srcOff + wordByteSize - n) .|. (word `shiftL` 8)
                    | otherwise = word
              in
              contin srcOff' $ \ t bs' ->
                DecodeResult t (Just $ bytesToWord wordByteSize 0) bs' srcOff'

decodeChunksProxy :: (Bits w, Num w, Storable w)
  => ( Int
    -> ByteString
    -> A.MArray s
    -> Int
    -> Int
    -> Int
    -> ST s (Maybe (
        (A.MArray s -> Int -> Int -> Int -> ST s (DecodeResult Text ByteString w))
        -> ST s (DecodeResult Text ByteString w)
        )
      )
    )
  -> ((Int -> Word8) -> Int -> Int -> Progression)
  -> ByteString
  -> ByteString
  -> ST s (DecodeResult Text ByteString w)
decodeChunksProxy = decodeChunks undefined -- This allows Haskell to
-- determine the size in bytes of a data type using Storable.sizeOf
-- so that it doesn't have to be passed as an arugment. Storable.sizeOf
-- discards the actual value without evaluating it.

bulkCharCopy
  :: Ptr Word8
  -> Int
  -> A.MArray s
  -> Int
  -> Int
  -> IO (A.MArray s, Int, Int)
bulkCharCopy src srcLen dst dstLen dstOff = do
  let minLen = dstOff + srcLen
  (dst', dstLen') <-
    if minLen > dstLen
      then
        let newLen = minLen + 4 in
        (, newLen) <$> (unsafeSTToIO $ A.resizeM dst newLen)
      else pure (dst, dstLen)
  unsafeSTToIO $ A.copyFromPointer dst' dstOff src srcLen
  pure (dst', dstLen', minLen)

-- | Decode two 'ByteString's containing ASCII as though they were one
-- continuous 'ByteString' returning a 'DecodeResult'.
--
-- @since 2.0.2
decodeAsciiChunks
  :: ByteString -- ^ The first 'ByteString' chunk to decode. Typically this is the unencoded data from the previous call of this function.
  -> ByteString -- ^ The second 'ByteString' chunk to decode.
  -> DecodeResult Text ByteString Word8
decodeAsciiChunks bs1 bs2 = runST $ decodeChunksProxy (
  \ bsOff bs dst dstLen srcOff dstOff -> withBS bs $ \ fp len ->
    unsafeIOToST . unsafeWithForeignPtr fp $ \src -> do
      let srcOff' = srcOff - bsOff
      asciiPrefixLen <- fmap fromIntegral . c_is_ascii (src `plusPtr` srcOff') $ src `plusPtr` len
      if asciiPrefixLen == 0
        then pure Nothing
        else do
          (dst', dstLen', dstOff') <- bulkCharCopy src asciiPrefixLen dst dstLen dstOff
          pure . Just $ \ f -> f dst' dstLen' (srcOff' + asciiPrefixLen + bsOff) dstOff'
  ) (\ _ _ _ -> Invalid) bs1 bs2

decodeUtf8Bulk
  :: Int
  -> ByteString
  -> A.MArray s
  -> Int
  -> Int
  -> Int
  -> ST s (Maybe ((A.MArray s -> Int -> Int -> Int -> t) -> t))
decodeUtf8Bulk bsOff bs dst dstLen srcOff dstOff
  -- potential valid utf8 content endpoint
  | utf8End <- bsOff + guessUtf8Boundary bs
  -- not yet reached the UTF-8 boundary
  , srcOff < utf8End
  -- potential valid utf8 content length
  , utf8Len <- utf8End - srcOff
  -- this ByteString chunk's offset
  , srcOff2 <- srcOff - bsOff
  -- check for UTF-8 compliance
  , isValidBS bs srcOff2 utf8Len = do
    (dst', dstLen', dstOff') <- withBS bs $ \ fp _ ->
      unsafeIOToST $ unsafeWithForeignPtr fp $ \ src ->
        bulkCharCopy (src `plusPtr` srcOff2) utf8Len dst dstLen dstOff
    pure . Just $ \ f -> f dst' dstLen' (srcOff + utf8Len) dstOff'
  | otherwise = pure Nothing
  where
    -- We need Data.ByteString.findIndexEnd, but it is unavailable before bytestring-0.10.12.0
    guessUtf8Boundary :: ByteString -> Int
    guessUtf8Boundary bs'@(B.length -> len)
      | len >= 1 && wr 1 < 0x80 -- last char is ASCII
        || wb 2 0xe0 0xc0       -- last two chars are a two-word code point
        || wb 3 0xf0 0xe0       -- last three chars are a three-word code point
        || wb 2 0xf8 0xf0       -- last four chars are a four-word code point
        = len
      | w 1 0xc0 = len - 1      -- last char starts a two-(or more-)word code point
      | w 2 0xe0 = len - 2      -- pre-last char starts a three- or four-word code point
      | w 3 0xf0 = len - 3      -- third to last char starts a four-word code point
      | otherwise = 0           -- gonna have to resolve this with an incremental approach
      where
        wr n = B.index bs' (len - n)
        w n bt = len >= n && wr n >= bt
        wb n mask bt = len >= n && wr n .&. mask == bt

    isValidBS :: ByteString -> Int -> Int -> Bool
    isValidBS bs' off count = if off + count > B.length bs'
      then False
      else
#ifdef SIMDUTF
        withBS bs' $ \ fp _ -> unsafeDupablePerformIO $
          unsafeWithForeignPtr fp $ \ ptr -> (/= 0) <$> c_is_valid_utf8 (ptr `plusPtr` off) (fromIntegral count)
#else
#if MIN_VERSION_bytestring(0,11,2)
        B.isValidUtf8 (B.take count $ B.drop off bs')
#else
        start off
      where
        start ix
          | ix >= off + count = True
          | otherwise = case utf8DecodeStart (B.unsafeIndex bs' ix) of
            Accept{} -> start (ix + 1)
            Reject{} -> False
            Incomplete st _ -> step (ix + 1) st
        step ix st
          | ix >= off + count = False
          -- We do not use decoded code point, so passing a dummy value to save an argument.
          | otherwise = case utf8DecodeContinue (B.unsafeIndex bs' ix) st (CodePoint 0) of
            Accept{} -> start (ix + 1)
            Reject{} -> False
            Incomplete st' _ -> step (ix + 1) st'
#endif
#endif

-- | Decode two 'ByteString's containing UTF-8-encoded text as though
-- they were one continuous 'ByteString' returning a 'DecodeResult'.
--
-- @since 2.0.2
decodeUtf8Chunks
  :: ByteString -- ^ The first 'ByteString' chunk to decode. Typically this is the unencoded data from the previous call of this function.
  -> ByteString -- ^ The second 'ByteString' chunk to decode.
  -> DecodeResult Text ByteString Word8
decodeUtf8Chunks bs1 bs2 = runST $ decodeChunksProxy decodeUtf8Bulk (\ index len srcOff ->
  let step i (Incomplete a b)
        | i < len = step (i + 1) $ utf8DecodeContinue (index i) a b
      step i st = (st, i)
  in
  case step (srcOff + 1) . utf8DecodeStart $ index srcOff of
    (Accept c, srcOff') -> WriteAndAdvance c srcOff'
    (Reject, _) -> Invalid
    (Incomplete{}, _) -> NeedMore) bs1 bs2

noBulkDecodeOp :: Applicative f => p0 -> p1 -> p2 -> p3 -> p4 -> p5 -> f (Maybe a)
noBulkDecodeOp _ _ _ _ _ _ = pure Nothing

-- | Decode two 'ByteString's containing UTF-16-encoded text as though
-- they were one continuous 'ByteString' returning a 'DecodeResult'.
--
-- @since 2.0.2
decodeUtf16Chunks
  :: Bool       -- ^ Indicates whether the encoding is big-endian ('True') or little-endian ('False')
  -> ByteString -- ^ The first 'ByteString' chunk to decode. Typically this is the unencoded data from the previous call of this function.
  -> ByteString -- ^ The second 'ByteString' chunk to decode.
  -> DecodeResult Text ByteString Word16
decodeUtf16Chunks isBE bs1 bs2 = runST $ decodeChunksProxy noBulkDecodeOp (\ index len srcOff ->
  -- get next Word8 pair
  let writeAndAdvance c n = WriteAndAdvance c $ srcOff + n
      b0 = index $ if isBE then srcOff else srcOff + 1
      b1 = index $ if isBE then srcOff + 1 else srcOff
  in
  case queryUtf16Bytes b0 b1 of
    OneWord16 c -> writeAndAdvance c 2
    TwoWord16 g ->
      if len - srcOff < 4
        -- not enough Word8s to finish the code point
        then NeedMore
        else
          let b2 = index $ srcOff + (if isBE then 2 else 3)
              b3 = index $ srcOff + (if isBE then 3 else 2)
          in
          case g b2 b3 of
            Just c -> writeAndAdvance c 4
            _ -> Invalid
    _ -> Invalid) bs1 bs2

-- | Decode two 'ByteString's containing UTF-16-encoded text as though
-- they were one continuous 'ByteString' returning a 'DecodeResult'.
--
-- @since 2.0.2
decodeUtf32Chunks
  :: Bool       -- ^ Indicates whether the encoding is big-endian ('True') or little-endian ('False')
  -> ByteString -- ^ The first 'ByteString' chunk to decode. Typically this is the unencoded data from the previous call of this function.
  -> ByteString -- ^ The second 'ByteString' chunk to decode.
  -> DecodeResult Text ByteString Word32
decodeUtf32Chunks isBE bs1 bs2 = runST $ decodeChunksProxy noBulkDecodeOp (\ index _ srcOff ->
  -- get next Word8 quartet
  case queryUtf32Bytes (index $ if isBE then srcOff else srcOff + 3)
      (index $ srcOff + (if isBE then 1 else 2))
      (index $ srcOff + (if isBE then 2 else 1))
      (index $ if isBE then srcOff + 3 else srcOff) of
    Just c -> WriteAndAdvance c $ srcOff + 4
    _ -> Invalid) bs1 bs2

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- Surrogate code points in replacement character returned by 'OnDecodeError'
-- will be automatically remapped to the replacement char @U+FFFD@.
decodeUtf8With ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> ByteString -> Text
decodeUtf8With onErr bs = case streamDecodeUtf8With onErr bs of
  Some t unencoded _ -> codePointToInvalid unencoded t
  where
    codePointToInvalid bs' txt =
      case B.uncons bs' of
        Just (x, bs'') -> codePointToInvalid bs'' $ case onErr desc $ Just x of
          Just c -> append txt $ T.singleton c
          _ -> txt
        _ -> txt

    desc = "Data.Text.Internal.Encoding: Incomplete UTF-8 code point"

-- $stream
--
-- The 'streamDecodeUtf8' and 'streamDecodeUtf8With' functions accept
-- a 'ByteString' that represents a possibly incomplete input (e.g. a
-- packet from a network stream) that may not end on a UTF-8 boundary.
--
-- 1. The maximal prefix of 'Text' that could be decoded from the
--    given input.
--
-- 2. The suffix of the 'ByteString' that could not be decoded due to
--    insufficient input.
--
-- 3. A function that accepts another 'ByteString'.  That string will
--    be assumed to directly follow the string that was passed as
--    input to the original function, and it will in turn be decoded.
--
-- To help understand the use of these functions, consider the Unicode
-- string @\"hi &#9731;\"@. If encoded as UTF-8, this becomes @\"hi
-- \\xe2\\x98\\x83\"@; the final @\'&#9731;\'@ is encoded as 3 bytes.
--
-- Now suppose that we receive this encoded string as 3 packets that
-- are split up on untidy boundaries: @[\"hi \\xe2\", \"\\x98\",
-- \"\\x83\"]@. We cannot decode the entire Unicode string until we
-- have received all three packets, but we would like to make progress
-- as we receive each one.
--
-- @
-- ghci> let s0\@('Some' _ _ f0) = 'streamDecodeUtf8' \"hi \\xe2\"
-- ghci> s0
-- 'Some' \"hi \" \"\\xe2\" _
-- @
--
-- We use the continuation @f0@ to decode our second packet.
--
-- @
-- ghci> let s1\@('Some' _ _ f1) = f0 \"\\x98\"
-- ghci> s1
-- 'Some' \"\" \"\\xe2\\x98\"
-- @
--
-- We could not give @f0@ enough input to decode anything, so it
-- returned an empty string. Once we feed our second continuation @f1@
-- the last byte of input, it will make progress.
--
-- @
-- ghci> let s2\@('Some' _ _ f2) = f1 \"\\x83\"
-- ghci> s2
-- 'Some' \"\\x2603\" \"\" _
-- @
--
-- If given invalid input, an exception will be thrown by the function
-- or continuation where it is encountered.

-- | A stream oriented decoding result.
--
-- @since 1.0.0.0
data Decoding = Some !Text !ByteString (ByteString -> Decoding)

instance Show Decoding where
    showsPrec d (Some t bs _) = showParen (d > prec) $
                                showString "Some " . showsPrec prec' t .
                                showChar ' ' . showsPrec prec' bs .
                                showString " _"
      where prec = 10; prec' = prec + 1

-- | Decode, in a stream oriented way, a 'ByteString' containing UTF-8
-- encoded text that is known to be valid.
--
-- If the input contains any invalid UTF-8 data, an exception will be
-- thrown (either by this function or a continuation) that cannot be
-- caught in pure code.  For more control over the handling of invalid
-- data, use 'streamDecodeUtf8With'.
--
-- @since 1.0.0.0
streamDecodeUtf8 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Decoding
streamDecodeUtf8 = streamDecodeUtf8With strictDecode

-- | Decode, in a stream oriented way, a 'ByteString' containing UTF-8
-- encoded text.
--
-- @since 1.0.0.0
streamDecodeUtf8With ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> ByteString -> Decoding
streamDecodeUtf8With onErr = g empty mempty
  where
    g t bs0 bs1 =
      let DecodeResult t' mW bs1' _ = decodeUtf8Chunks bs0 bs1
          txt = t `append` t'
      in
      (case (mW :: Maybe Word8) of
        Just _ ->
          g (case onErr "" mW of
            Just c -> txt `append` T.singleton c
            _ -> txt) mempty
        _ -> Some txt bs1' . g empty) bs1'

-- | Decode a 'ByteString' containing UTF-8 encoded text that is known
-- to be valid.
--
-- If the input contains any invalid UTF-8 data, an exception will be
-- thrown that cannot be caught in pure code.  For more control over
-- the handling of invalid data, use 'decodeUtf8'' or
-- 'decodeUtf8With'.
--
-- This is a partial function: it checks that input is a well-formed
-- UTF-8 sequence and copies buffer or throws an error otherwise.
--
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With strictDecode
{-# INLINE[0] decodeUtf8 #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- If the input contains any invalid UTF-8 data, the relevant
-- exception will be returned, otherwise the decoded text.
decodeUtf8' ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Either UnicodeException Text
decodeUtf8' = unsafeDupablePerformIO . try . evaluate . decodeUtf8With strictDecode
{-# INLINE decodeUtf8' #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- Any invalid input bytes will be replaced with the Unicode replacement
-- character U+FFFD.
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

-- | Encode text to a ByteString 'B.Builder' using UTF-8 encoding.
--
-- @since 1.1.0.0
encodeUtf8Builder :: Text -> B.Builder
encodeUtf8Builder =
    -- manual eta-expansion to ensure inlining works as expected
    \txt -> B.builder (step txt)
  where
    step txt@(Text arr off len) !k br@(B.BufferRange op ope)
      -- Ensure that the common case is not recursive and therefore yields
      -- better code.
      | op' <= ope = do
          unsafeSTToIO $ A.copyToPointer arr off op len
          k (B.BufferRange op' ope)
      | otherwise = textCopyStep txt k br
      where
        op' = op `plusPtr` len
{-# INLINE encodeUtf8Builder #-}

textCopyStep :: Text -> B.BuildStep a -> B.BuildStep a
textCopyStep (Text arr off len) k =
    go off (off + len)
  where
    go !ip !ipe (B.BufferRange op ope)
      | inpRemaining <= outRemaining = do
          unsafeSTToIO $ A.copyToPointer arr ip op inpRemaining
          let !br = B.BufferRange (op `plusPtr` inpRemaining) ope
          k br
      | otherwise = do
          unsafeSTToIO $ A.copyToPointer arr ip op outRemaining
          let !ip' = ip + outRemaining
          return $ B.bufferFull 1 ope (go ip' ipe)
      where
        outRemaining = ope `minusPtr` op
        inpRemaining = ipe - ip

-- | Encode text using UTF-8 encoding and escape the ASCII characters using
-- a 'BP.BoundedPrim'.
--
-- Use this function is to implement efficient encoders for text-based formats
-- like JSON or HTML.
--
-- @since 1.1.0.0
{-# INLINE encodeUtf8BuilderEscaped #-}
-- TODO: Extend documentation with references to source code in @blaze-html@
-- or @aeson@ that uses this function.
encodeUtf8BuilderEscaped :: BP.BoundedPrim Word8 -> Text -> B.Builder
encodeUtf8BuilderEscaped be =
    -- manual eta-expansion to ensure inlining works as expected
    \txt -> B.builder (mkBuildstep txt)
  where
    bound = max 4 $ BP.sizeBound be

    mkBuildstep (Text arr off len) !k =
        outerLoop off
      where
        iend = off + len

        outerLoop !i0 !br@(B.BufferRange op0 ope)
          | i0 >= iend       = k br
          | outRemaining > 0 = goPartial (i0 + min outRemaining inpRemaining)
          -- TODO: Use a loop with an integrated bound's check if outRemaining
          -- is smaller than 8, as this will save on divisions.
          | otherwise        = return $ B.bufferFull bound op0 (outerLoop i0)
          where
            outRemaining = (ope `minusPtr` op0) `quot` bound
            inpRemaining = iend - i0

            goPartial !iendTmp = go i0 op0
              where
                go !i !op
                  | i < iendTmp = do
                    let w = A.unsafeIndex arr i
                    if w < 0x80
                      then BP.runB be w op >>= go (i + 1)
                      else poke op w >> go (i + 1) (op `plusPtr` 1)
                  | otherwise = outerLoop i (B.BufferRange op ope)

-- | Encode text using UTF-8 encoding.
encodeUtf8 :: Text -> ByteString
encodeUtf8 (Text arr off len)
  | len == 0  = B.empty
  -- It would be easier to use Data.ByteString.Short.fromShort and slice later,
  -- but this is undesirable when len is significantly smaller than length arr.
  | otherwise = unsafeDupablePerformIO $ do
    marr@(A.MutableByteArray mba) <- unsafeSTToIO $ A.newPinned len
    unsafeSTToIO $ A.copyI len marr 0 arr off
    let fp = ForeignPtr (byteArrayContents# (unsafeCoerce# mba))
                        (PlainPtr mba)
    pure $ B.fromForeignPtr fp 0 len

-- | Decode text from little endian UTF-16 encoding.
decodeUtf16LEWith :: OnDecodeError -> ByteString -> Text
decodeUtf16LEWith onErr bs = F.unstream (E.streamUtf16LE onErr bs)
{-# INLINE decodeUtf16LEWith #-}

-- | Decode text from little endian UTF-16 encoding.
--
-- If the input contains any invalid little endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16LEWith'.
decodeUtf16LE :: ByteString -> Text
decodeUtf16LE = decodeUtf16LEWith strictDecode
{-# INLINE decodeUtf16LE #-}

-- | Decode text from big endian UTF-16 encoding.
decodeUtf16BEWith :: OnDecodeError -> ByteString -> Text
decodeUtf16BEWith onErr bs = F.unstream (E.streamUtf16BE onErr bs)
{-# INLINE decodeUtf16BEWith #-}

-- | Decode text from big endian UTF-16 encoding.
--
-- If the input contains any invalid big endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16BEWith'.
decodeUtf16BE :: ByteString -> Text
decodeUtf16BE = decodeUtf16BEWith strictDecode
{-# INLINE decodeUtf16BE #-}

-- | Encode text using little endian UTF-16 encoding.
encodeUtf16LE :: Text -> ByteString
encodeUtf16LE txt = E.unstream (E.restreamUtf16LE (F.stream txt))
{-# INLINE encodeUtf16LE #-}

-- | Encode text using big endian UTF-16 encoding.
encodeUtf16BE :: Text -> ByteString
encodeUtf16BE txt = E.unstream (E.restreamUtf16BE (F.stream txt))
{-# INLINE encodeUtf16BE #-}

-- | Decode text from little endian UTF-32 encoding.
decodeUtf32LEWith :: OnDecodeError -> ByteString -> Text
decodeUtf32LEWith onErr bs = F.unstream (E.streamUtf32LE onErr bs)
{-# INLINE decodeUtf32LEWith #-}

-- | Decode text from little endian UTF-32 encoding.
--
-- If the input contains any invalid little endian UTF-32 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf32LEWith'.
decodeUtf32LE :: ByteString -> Text
decodeUtf32LE = decodeUtf32LEWith strictDecode
{-# INLINE decodeUtf32LE #-}

-- | Decode text from big endian UTF-32 encoding.
decodeUtf32BEWith :: OnDecodeError -> ByteString -> Text
decodeUtf32BEWith onErr bs = F.unstream (E.streamUtf32BE onErr bs)
{-# INLINE decodeUtf32BEWith #-}

-- | Decode text from big endian UTF-32 encoding.
--
-- If the input contains any invalid big endian UTF-32 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf32BEWith'.
decodeUtf32BE :: ByteString -> Text
decodeUtf32BE = decodeUtf32BEWith strictDecode
{-# INLINE decodeUtf32BE #-}

-- | Encode text using little endian UTF-32 encoding.
encodeUtf32LE :: Text -> ByteString
encodeUtf32LE txt = E.unstream (E.restreamUtf32LE (F.stream txt))
{-# INLINE encodeUtf32LE #-}

-- | Encode text using big endian UTF-32 encoding.
encodeUtf32BE :: Text -> ByteString
encodeUtf32BE txt = E.unstream (E.restreamUtf32BE (F.stream txt))
{-# INLINE encodeUtf32BE #-}

#ifdef SIMDUTF
foreign import ccall unsafe "_hs_text_is_valid_utf8" c_is_valid_utf8
    :: Ptr Word8 -> CSize -> IO CInt
#endif
