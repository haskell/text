{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , decodeASCIIPrefix
    , decodeUtf8Lenient
    , decodeUtf8'
    , decodeASCII'

    -- *** Controllable error handling
    , decodeUtf8With
    , decodeUtf16LEWith
    , decodeUtf16BEWith
    , decodeUtf32LEWith
    , decodeUtf32BEWith

    -- *** Stream oriented decoding
    -- $stream
    , streamDecodeUtf8With
    , Decoding(..)

    -- *** Incremental UTF-8 decoding
    -- $incremental
    , decodeUtf8Chunk
    , decodeUtf8More
    , Utf8State
    , startUtf8State
    , StrictBuilder
    , strictBuilderToText
    , textToStrictBuilder

    -- ** Partial Functions
    -- $partial
    , decodeASCII
    , decodeUtf8
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE

    -- *** Stream oriented decoding
    , streamDecodeUtf8

    -- * Encoding Text to ByteStrings
    , encodeUtf8
    , encodeUtf16LE
    , encodeUtf16BE
    , encodeUtf32LE
    , encodeUtf32BE

    -- * Encoding Text using ByteString Builders
    , encodeUtf8Builder
    , encodeUtf8BuilderEscaped

    -- * ByteString validation
    -- $validation
    , validateUtf8Chunk
    , validateUtf8More
    ) where

import Control.Exception (evaluate, try)
import Data.Word (Word8)
import GHC.Exts (byteArrayContents#, unsafeCoerce#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(PlainPtr))
import Data.ByteString (ByteString)
#if defined(PURE_HASKELL)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.ByteString.Char8 (unpack)
import Data.Text.Internal (pack)
import Foreign.Ptr (minusPtr, plusPtr)
import Foreign.Storable (poke)
#else
import Control.Monad.ST (runST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.Bits (shiftR, (.&.))
import Data.Text.Internal.ByteStringCompat (withBS)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (poke, peekByteOff)
#endif
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException, strictDecode, lenientDecode)
import Data.Text.Internal (Text(..), empty)
import Data.Text.Internal.Encoding
import Data.Text.Internal.IsAscii (asciiPrefixLength)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Text.Show ()
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Internal as B hiding (empty, append)
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder.Prim.Internal as BP
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Encoding.Fusion as E
import qualified Data.Text.Internal.Fusion as F
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

-- $validation
-- These functions are for validating 'ByteString's as encoded text.

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

-- | Decode a 'ByteString' containing ASCII text.
--
-- This is a total function which returns a pair of the longest ASCII prefix
-- as 'Text', and the remaining suffix as 'ByteString'.
--
-- Important note: the pair is lazy. This lets you check for errors by testing
-- whether the second component is empty, without forcing the first component
-- (which does a copy).
-- To drop references to the input bytestring, force the prefix
-- (using 'seq' or @BangPatterns@) and drop references to the suffix.
--
-- === Properties
--
-- - If @(prefix, suffix) = decodeAsciiPrefix s@, then @'encodeUtf8' prefix <> suffix = s@.
-- - Either @suffix@ is empty, or @'B.head' suffix > 127@.
--
-- @since 2.0.2
decodeASCIIPrefix :: ByteString -> (Text, ByteString)
decodeASCIIPrefix bs = if B.null bs
  then (empty, B.empty)
  else
    let len = asciiPrefixLength bs
        prefix =
          let !(SBS.SBS arr) = SBS.toShort (B.take len bs) in
          Text (A.ByteArray arr) 0 len
        suffix = B.drop len bs in
    (prefix, suffix)
{-# INLINE decodeASCIIPrefix #-}

-- | Decode a 'ByteString' containing 7-bit ASCII encoded text.
--
-- This is a total function which returns either the 'ByteString' converted to a
-- 'Text' containing ASCII text, or 'Nothing'.
--
-- Use 'decodeASCIIPrefix' to retain the longest ASCII prefix for an invalid
-- input instead of discarding it.
--
-- @since 2.0.2
decodeASCII' :: ByteString -> Maybe Text
decodeASCII' bs =
  let (prefix, suffix) = decodeASCIIPrefix bs in
  if B.null suffix then Just prefix else Nothing
{-# INLINE decodeASCII' #-}

-- | Decode a 'ByteString' containing 7-bit ASCII encoded text.
--
-- This is a partial function: it checks that input does not contain
-- anything except ASCII and copies buffer or throws an error otherwise.
decodeASCII :: ByteString -> Text
decodeASCII bs =
  let (prefix, suffix) = decodeASCIIPrefix bs in
  case B.uncons suffix of
    Nothing -> prefix
    Just (word, _) ->
      let !errPos = B.length bs - B.length suffix in
      error $ "decodeASCII: detected non-ASCII codepoint " ++ show word ++ " at position " ++ show errPos

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
#if defined(PURE_HASKELL)
decodeLatin1 bs = pack (Data.ByteString.Char8.unpack bs)
#else
decodeLatin1 bs = withBS bs $ \fp len -> runST $ do
  dst <- A.new (2 * len)
  let inner srcOff dstOff = if srcOff >= len then return dstOff else do
        asciiPrefixLen <- fmap fromIntegral $ unsafeIOToST $ unsafeWithForeignPtr fp $ \src ->
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
#endif

#if !defined(PURE_HASKELL)
foreign import ccall unsafe "_hs_text_is_ascii" c_is_ascii
    :: Ptr Word8 -> Ptr Word8 -> IO CSize
#endif

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

-- | Decode, in a stream oriented way, a lazy 'ByteString' containing UTF-8
-- encoded text.
--
-- @since 1.0.0.0
streamDecodeUtf8With ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> ByteString -> Decoding
streamDecodeUtf8With onErr = loop startUtf8State
  where
    loop s chunk =
      let (builder, undecoded, s') = decodeUtf8With2 onErr invalidUtf8Msg s chunk
      in Some (strictBuilderToText builder) undecoded (loop s')

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- Surrogate code points in replacement character returned by 'OnDecodeError'
-- will be automatically remapped to the replacement char @U+FFFD@.
decodeUtf8With ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> ByteString -> Text
decodeUtf8With onErr = decodeUtf8With1 onErr invalidUtf8Msg

invalidUtf8Msg :: String
invalidUtf8Msg = "Data.Text.Encoding: Invalid UTF-8 stream"

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

-- $incremental
-- The functions 'decodeUtf8Chunk' and 'decodeUtf8More' provide more
-- control for error-handling and streaming.
--
-- - Those functions return an UTF-8 prefix of the given 'ByteString' up to the next error.
--   For example this lets you insert or delete arbitrary text, or do some
--   stateful operations before resuming, such as keeping track of error locations.
--   In contrast, the older stream-oriented interface only lets you substitute
--   a single fixed 'Char' for each invalid byte in 'OnDecodeError'.
-- - That prefix is encoded as a 'StrictBuilder', so you can accumulate chunks
--   before doing the copying work to construct a 'Text', or you can
--   output decoded fragments immediately as a lazy 'Data.Text.Lazy.Text'.
--
-- For even lower-level primitives, see 'validateUtf8Chunk' and 'validateUtf8More'.
