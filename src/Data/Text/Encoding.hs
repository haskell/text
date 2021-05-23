{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
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
      decodeASCII
    , decodeLatin1
    , decodeUtf8
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE

    -- ** Catchable failure
    , decodeUtf8'

    -- ** Controllable error handling
    , decodeUtf8With
    , decodeUtf8Lenient
    , decodeUtf16LEWith
    , decodeUtf16BEWith
    , decodeUtf32LEWith
    , decodeUtf32BEWith

    -- ** Stream oriented decoding
    -- $stream
    , streamDecodeUtf8
    , streamDecodeUtf8With
    , Decoding(..)

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

import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)

import Control.Exception (evaluate, try, throwIO, ErrorCall(ErrorCall))
import Control.Monad.ST (runST)
import Data.ByteString as B
import qualified Data.ByteString.Short.Internal as SBS
import Data.Foldable (traverse_)
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException, strictDecode, lenientDecode)
import Data.Text.Internal (Text(..), safe, text)
import Data.Text.Internal.Private (runText)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Data.Text.Internal.Unsafe.Char (unsafeWrite)
import Data.Text.Show ()
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word8, Word32)
import Foreign.C.Types (CSize)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable, peek, poke)
import GHC.Base (MutableByteArray#)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Internal as B hiding (empty, append)
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder.Prim.Internal as BP
import Data.Text.Internal.Encoding.Utf8 (utf8LengthByLeader)
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Encoding.Fusion as E
import qualified Data.Text.Internal.Fusion as F
import Data.Text.Internal.ByteStringCompat
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

#include "text_cbits.h"

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

-- | /Deprecated/.  Decode a 'ByteString' containing 7-bit ASCII
-- encoded text.
decodeASCII :: ByteString -> Text
decodeASCII = decodeUtf8
{-# DEPRECATED decodeASCII "Use decodeUtf8 instead" #-}

-- | Decode a 'ByteString' containing Latin-1 (aka ISO-8859-1) encoded text.
--
-- 'decodeLatin1' is semantically equivalent to
--  @Data.Text.pack . Data.ByteString.Char8.unpack@
decodeLatin1 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Text
decodeLatin1 bs = withBS bs aux where
  aux fp len = text a 0 actualLen
   where
    (a, actualLen) = A.run2 (A.new (2 * len) >>= unsafeIOToST . go)
    go (A.MutableByteArray dest) = unsafeWithForeignPtr fp $ \src -> do
      destLen <- c_decode_latin1 dest src (src `plusPtr` len)
      return (A.MutableByteArray dest, destLen)

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- __NOTE__: The replacement character returned by 'OnDecodeError'
-- MUST be within the BMP plane; surrogate code points will
-- automatically be remapped to the replacement char @U+FFFD@
-- (/since 0.11.3.0/), whereas code points beyond the BMP will throw an
-- 'error' (/since 1.2.3.1/); For earlier versions of @text@ using
-- those unsupported code points would result in undefined behavior.
decodeUtf8With ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> ByteString -> Text
decodeUtf8With onErr bs = withBS bs aux
 where
  aux fp len = runText $ \done -> do
    let go (A.MutableByteArray dest) = unsafeWithForeignPtr fp $ \ptr ->
          with (0::CSize) $ \destOffPtr -> do
            let end = ptr `plusPtr` len
                loop curPtr = do
                  curPtr' <- c_decode_utf8 dest destOffPtr curPtr end
                  if curPtr' == end
                    then do
                      n <- peek destOffPtr
                      unsafeSTToIO (done (A.MutableByteArray dest) (cSizeToInt n))
                    else do
                      x <- peek curPtr'
                      case onErr desc (Just x) of
                        Nothing -> loop $ curPtr' `plusPtr` 1
                        Just c
                          -- TODO This is problematic, because even BMP replacement characters
                          -- can take longer than one UTF8 code unit (which is byte).
                          | c > '\xFFFF' -> throwUnsupportedReplChar
                          | otherwise -> do
                              destOff <- peek destOffPtr
                              w <- unsafeSTToIO $
                                   unsafeWrite (A.MutableByteArray dest) (cSizeToInt destOff)
                                               (safe c)
                              poke destOffPtr (destOff + intToCSize w)
                              loop $ curPtr' `plusPtr` 1
            loop ptr
    -- TODO (len * 2 + 100) assumes that invalid input is asymptotically rare.
    -- This is incorrect in general, but for now we just want to pass tests.
    (unsafeIOToST . go) =<< A.new (len * 2 + 100)
   where
    desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"

    throwUnsupportedReplChar = throwIO $
      ErrorCall "decodeUtf8With: non-BMP replacement characters not supported"

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
data Decoding = Some Text ByteString (ByteString -> Decoding)

instance Show Decoding where
    showsPrec d (Some t bs _) = showParen (d > prec) $
                                showString "Some " . showsPrec prec' t .
                                showChar ' ' . showsPrec prec' bs .
                                showString " _"
      where prec = 10; prec' = prec + 1

newtype CodePoint = CodePoint Word32 deriving (Eq, Show, Num, Storable)
newtype DecoderState = DecoderState Word32 deriving (Eq, Show, Num, Storable)

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
streamDecodeUtf8With onErr = decodeChunk B.empty 0 0
 where
  -- We create a slightly larger than necessary buffer to accommodate a
  -- potential code point started in the last buffer (@undecoded0@), or
  -- replacement characters for each byte in @undecoded0@ if the
  -- sequence turns out to be invalid. There can be up to three bytes there,
  -- hence we allocate @len+3@ bytes.
  decodeChunk :: ByteString -> CodePoint -> DecoderState -> ByteString
              -> Decoding
  decodeChunk undecoded0 codepoint0 state0 bs = withBS bs aux where
    -- TODO Replace (+100) with something sensible.
    aux fp len = runST $ (unsafeIOToST . decodeChunkToBuffer) =<< A.new (len+100)
       where
        decodeChunkToBuffer :: A.MArray s -> IO Decoding
        decodeChunkToBuffer (A.MutableByteArray dest) = unsafeWithForeignPtr fp $ \ptr ->
          with (0::CSize) $ \destOffPtr ->
          with codepoint0 $ \codepointPtr ->
          with state0 $ \statePtr ->
          with nullPtr $ \curPtrPtr ->
            let end = ptr `plusPtr` len
                loop curPtr = do
                  prevState <- peek statePtr
                  poke curPtrPtr curPtr
                  lastPtr <- c_decode_utf8_with_state dest destOffPtr
                             curPtrPtr end codepointPtr statePtr
                  state <- peek statePtr
                  case state of
                    UTF8_REJECT -> do
                      -- We encountered an encoding error
                      poke statePtr 0
                      let skipByte x = case onErr desc (Just x) of
                            Nothing -> return ()
                            Just c -> do
                              destOff <- peek destOffPtr
                              w <- unsafeSTToIO $
                                   unsafeWrite (A.MutableByteArray dest) (cSizeToInt destOff) (safe c)
                              poke destOffPtr (destOff + intToCSize w)
                      if ptr == lastPtr && prevState /= UTF8_ACCEPT then do
                        -- If we can't complete the sequence @undecoded0@ from
                        -- the previous chunk, we invalidate the bytes from
                        -- @undecoded0@ and retry decoding the current chunk from
                        -- the initial state.
                        traverse_ skipByte (B.unpack undecoded0)
                        loop lastPtr
                      else do
                        peek lastPtr >>= skipByte
                        loop (lastPtr `plusPtr` 1)

                    _ -> do
                      -- We encountered the end of the buffer while decoding
                      n <- peek destOffPtr
                      codepoint <- peek codepointPtr
                      chunkText <- unsafeSTToIO $ do
                          let l = cSizeToInt n
                          A.shrinkM (A.MutableByteArray dest) l
                          arr <- A.unsafeFreeze (A.MutableByteArray dest)
                          return $! text arr 0 l
                      let left = lastPtr `minusPtr` ptr
                          !undecoded = case state of
                            UTF8_ACCEPT -> B.empty
                            _ | left == 0 && prevState /= UTF8_ACCEPT -> B.append undecoded0 bs
                              | otherwise -> B.drop left bs
                      return $ Some chunkText undecoded
                               (decodeChunk undecoded codepoint state)
            in loop ptr
  desc = "Data.Text.Internal.Encoding.streamDecodeUtf8With: Invalid UTF-8 stream"

-- | Decode a 'ByteString' containing UTF-8 encoded text that is known
-- to be valid.
--
-- If the input contains any invalid UTF-8 data, an exception will be
-- thrown that cannot be caught in pure code.  For more control over
-- the handling of invalid data, use 'decodeUtf8'' or
-- 'decodeUtf8With'.
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
encodeUtf8Builder = encodeUtf8BuilderEscaped (BP.liftFixedToBounded BP.word8)

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
            outRemaining = (ope `minusPtr` op0) `div` bound
            inpRemaining = iend - i0

            goPartial !iendTmp = go i0 op0
              where
                go !i !op
                  | i < iendTmp = case utf8LengthByLeader w of
                    1 -> do
                      BP.runB be w op >>= go (i + 1)
                    2 -> do
                      poke (op `plusPtr` 0) w
                      poke (op `plusPtr` 1) (A.unsafeIndex arr (i+1))
                      go (i + 2) (op `plusPtr` 2)
                    3 -> do
                      poke (op `plusPtr` 0) w
                      poke (op `plusPtr` 1) (A.unsafeIndex arr (i+1))
                      poke (op `plusPtr` 2) (A.unsafeIndex arr (i+2))
                      go (i + 3) (op `plusPtr` 3)
                    _ -> do
                      poke (op `plusPtr` 0) w
                      poke (op `plusPtr` 1) (A.unsafeIndex arr (i+1))
                      poke (op `plusPtr` 2) (A.unsafeIndex arr (i+2))
                      poke (op `plusPtr` 3) (A.unsafeIndex arr (i+3))
                      go (i + 4) (op `plusPtr` 4)
                  | otherwise = outerLoop i (B.BufferRange op ope)
                  where
                    w = A.unsafeIndex arr i

-- | Encode text using UTF-8 encoding.
encodeUtf8 :: Text -> ByteString
encodeUtf8 (Text (A.ByteArray arr) off len)
  | len == 0  = B.empty
  | otherwise = B.take len $ B.drop off $ SBS.fromShort $ SBS.SBS arr

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

cSizeToInt :: CSize -> Int
cSizeToInt = fromIntegral

intToCSize :: Int -> CSize
intToCSize = fromIntegral

foreign import ccall unsafe "_hs_text_decode_utf8" c_decode_utf8
    :: MutableByteArray# s -> Ptr CSize
    -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "_hs_text_decode_utf8_state" c_decode_utf8_with_state
    :: MutableByteArray# s -> Ptr CSize
    -> Ptr (Ptr Word8) -> Ptr Word8
    -> Ptr CodePoint -> Ptr DecoderState -> IO (Ptr Word8)

foreign import ccall unsafe "_hs_text_decode_latin1" c_decode_latin1
    :: MutableByteArray# s -> Ptr Word8 -> Ptr Word8 -> IO Int
