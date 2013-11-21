{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- |
-- Module      : Data.Text.Encoding
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts,
--               (c) 2008, 2009 Tom Harper
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
--
-- Functions for converting 'Text' values to and from 'ByteString',
-- using several standard encodings.
--
-- To gain access to a much larger family of encodings, use the
-- @text-icu@ package: <http://hackage.haskell.org/package/text-icu>

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
    ) where

import Control.Exception (evaluate, try)
#if __GLASGOW_HASKELL__ >= 702
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
#else
import Control.Monad.ST (unsafeIOToST, unsafeSTToIO)
#endif
import Control.Monad.ST (runST)
import Data.Bits ((.&.))
import Data.ByteString as B
import Data.ByteString.Internal as B
import Data.Text ()
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException, strictDecode)
import Data.Text.Internal (Text(..), safe, textP)
import Data.Text.Private (runText)
import Data.Text.UnsafeChar (ord, unsafeWrite)
import Data.Text.UnsafeShift (shiftL, shiftR)
import Data.Word (Word8, Word32)
import Foreign.C.Types (CSize)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable, peek, poke)
import GHC.Base (MutableByteArray#)
import qualified Data.Text.Array as A
import qualified Data.Text.Encoding.Fusion as E
import qualified Data.Text.Encoding.Utf16 as U16
import qualified Data.Text.Fusion as F
import Data.Text.Unsafe (unsafeDupablePerformIO)

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
--
-- This function is deprecated.  Use 'decodeLatin1' instead.
decodeASCII :: ByteString -> Text
decodeASCII = decodeUtf8
{-# DEPRECATED decodeASCII "Use decodeUtf8 instead" #-}

-- | Decode a 'ByteString' containing Latin-1 (aka ISO-8859-1) encoded text.
--
-- 'decodeLatin1' is semantically equivalent to
--  @Data.Text.pack . Data.ByteString.Char8.unpack@
decodeLatin1 :: ByteString -> Text
decodeLatin1 (PS fp off len) = textP a 0 len
 where
  a = A.run (A.new len >>= unsafeIOToST . go)
  go dest = withForeignPtr fp $ \ptr -> do
    c_decode_latin1 (A.maBA dest) (ptr `plusPtr` off) (ptr `plusPtr` (off+len))
    return dest

-- | Decode a 'ByteString' containing UTF-8 encoded text.
decodeUtf8With :: OnDecodeError -> ByteString -> Text
decodeUtf8With onErr (PS fp off len) = runText $ \done -> do
  let go dest = withForeignPtr fp $ \ptr ->
        with (0::CSize) $ \destOffPtr -> do
          let end = ptr `plusPtr` (off + len)
              loop curPtr = do
                curPtr' <- c_decode_utf8 (A.maBA dest) destOffPtr curPtr end
                if curPtr' == end
                  then do
                    n <- peek destOffPtr
                    unsafeSTToIO (done dest (fromIntegral n))
                  else do
                    x <- peek curPtr'
                    case onErr desc (Just x) of
                      Nothing -> loop $ curPtr' `plusPtr` 1
                      Just c -> do
                        destOff <- peek destOffPtr
                        w <- unsafeSTToIO $
                             unsafeWrite dest (fromIntegral destOff) (safe c)
                        poke destOffPtr (destOff + fromIntegral w)
                        loop $ curPtr' `plusPtr` 1
          loop (ptr `plusPtr` off)
  (unsafeIOToST . go) =<< A.new len
 where
  desc = "Data.Text.Encoding.decodeUtf8: Invalid UTF-8 stream"
{- INLINE[0] decodeUtf8With #-}

-- $stream
--
-- The 'streamDecodeUtf8' and 'streamDecodeUtf8With' functions accept
-- a 'ByteString' that represents a possibly incomplete input (e.g. a
-- packet from a network stream) that may not end on a UTF-8 boundary.
--
-- The first element of the result is the maximal chunk of 'Text' that
-- can be decoded from the given input. The second is a function which
-- accepts another 'ByteString'. That string will be assumed to
-- directly follow the string that was passed as input to the original
-- function, and it will in turn be decoded.
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
-- let 'Some' t0 f0 = 'streamDecodeUtf8' \"hi \\xe2\"
-- t0 == \"hi \" :: 'Text'
-- @
--
-- We use the continuation @f0@ to decode our second packet.
--
-- @
-- let 'Some' t1 f1 = f0 \"\\x98\"
-- t1 == \"\"
-- @
--
-- We could not give @f0@ enough input to decode anything, so it
-- returned an empty string. Once we feed our second continuation @f1@
-- the last byte of input, it will make progress.
--
-- @
-- let 'Some' t2 f2 = f1 \"\\x83\"
-- t2 == \"&#9731;\"
-- @
--
-- If given invalid input, an exception will be thrown by the function
-- or continuation where it is encountered.

-- | A stream oriented decoding result.
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
streamDecodeUtf8 :: ByteString -> Decoding
streamDecodeUtf8 = streamDecodeUtf8With strictDecode

-- | Decode, in a stream oriented way, a 'ByteString' containing UTF-8
-- encoded text.
streamDecodeUtf8With :: OnDecodeError -> ByteString -> Decoding
streamDecodeUtf8With onErr = decodeChunk 0 0
 where
  -- We create a slightly larger than necessary buffer to accommodate a
  -- potential surrogate pair started in the last buffer
  decodeChunk :: CodePoint -> DecoderState -> ByteString -> Decoding
  decodeChunk codepoint0 state0 bs@(PS fp off len) =
    runST $ (unsafeIOToST . decodeChunkToBuffer) =<< A.new (len+1)
   where
    decodeChunkToBuffer :: A.MArray s -> IO Decoding
    decodeChunkToBuffer dest = withForeignPtr fp $ \ptr ->
      with (0::CSize) $ \destOffPtr ->
      with codepoint0 $ \codepointPtr ->
      with state0 $ \statePtr ->
      with nullPtr $ \curPtrPtr ->
        let end = ptr `plusPtr` (off + len)
            loop curPtr = do
              poke curPtrPtr curPtr
              curPtr' <- c_decode_utf8_with_state (A.maBA dest) destOffPtr
                         curPtrPtr end codepointPtr statePtr
              state <- peek statePtr
              case state of
                UTF8_REJECT -> do
                  -- We encountered an encoding error
                  x <- peek curPtr'
                  case onErr desc (Just x) of
                    Nothing -> loop $ curPtr' `plusPtr` 1
                    Just c -> do
                      destOff <- peek destOffPtr
                      w <- unsafeSTToIO $
                           unsafeWrite dest (fromIntegral destOff) (safe c)
                      poke destOffPtr (destOff + fromIntegral w)
                      poke statePtr 0
                      loop $ curPtr' `plusPtr` 1

                _ -> do
                  -- We encountered the end of the buffer while decoding
                  n <- peek destOffPtr
                  codepoint <- peek codepointPtr
                  chunkText <- unsafeSTToIO $ do
                      arr <- A.unsafeFreeze dest
                      return $! textP arr 0 (fromIntegral n)
                  lastPtr <- peek curPtrPtr
                  let left = lastPtr `minusPtr` curPtr
                  return $ Some chunkText (B.drop left bs)
                           (decodeChunk codepoint state)
        in loop (ptr `plusPtr` off)
  desc = "Data.Text.Encoding.streamDecodeUtf8With: Invalid UTF-8 stream"

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
{-# RULES "STREAM stream/decodeUtf8 fusion" [1]
    forall bs. F.stream (decodeUtf8 bs) = E.streamUtf8 strictDecode bs #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- If the input contains any invalid UTF-8 data, the relevant
-- exception will be returned, otherwise the decoded text.
decodeUtf8' :: ByteString -> Either UnicodeException Text
decodeUtf8' = unsafeDupablePerformIO . try . evaluate . decodeUtf8With strictDecode
{-# INLINE decodeUtf8' #-}

-- | Encode text using UTF-8 encoding.
encodeUtf8 :: Text -> ByteString
encodeUtf8 (Text arr off len) = unsafeDupablePerformIO $ do
  let size0 = max len 4
  mallocByteString size0 >>= start size0 off 0
 where
  start size n0 m0 fp = withForeignPtr fp $ loop n0 m0
   where
    loop n1 m1 ptr = go n1 m1
     where
      offLen = off + len
      go !n !m
        | n == offLen = return (PS fp 0 m)
        | otherwise = do
            let poke8 k v = poke (ptr `plusPtr` k) (fromIntegral v :: Word8)
                ensure k act
                  | size-m >= k = act
                  | otherwise = {-# SCC "resizeUtf8/ensure" #-} do
                      let newSize = size `shiftL` 1
                      fp' <- mallocByteString newSize
                      withForeignPtr fp' $ \ptr' ->
                        memcpy ptr' ptr (fromIntegral m)
                      start newSize n m fp'
                {-# INLINE ensure #-}
            case A.unsafeIndex arr n of
             w| w <= 0x7F  -> ensure 1 $ do
                  poke (ptr `plusPtr` m) (fromIntegral w :: Word8)
                  -- A single ASCII octet is likely to start a run of
                  -- them.  We see better performance when we
                  -- special-case this assumption.
                  let end = ptr `plusPtr` size
                      ascii !t !u
                        | t == offLen || u == end || v >= 0x80 =
                            go t (u `minusPtr` ptr)
                        | otherwise = do
                            poke u (fromIntegral v :: Word8)
                            ascii (t+1) (u `plusPtr` 1)
                        where v = A.unsafeIndex arr t
                  ascii (n+1) (ptr `plusPtr` (m+1))
              | w <= 0x7FF -> ensure 2 $ do
                  poke8 m     $ (w `shiftR` 6) + 0xC0
                  poke8 (m+1) $ (w .&. 0x3f) + 0x80
                  go (n+1) (m+2)
              | 0xD800 <= w && w <= 0xDBFF -> ensure 4 $ do
                  let c = ord $ U16.chr2 w (A.unsafeIndex arr (n+1))
                  poke8 m     $ (c `shiftR` 18) + 0xF0
                  poke8 (m+1) $ ((c `shiftR` 12) .&. 0x3F) + 0x80
                  poke8 (m+2) $ ((c `shiftR` 6) .&. 0x3F) + 0x80
                  poke8 (m+3) $ (c .&. 0x3F) + 0x80
                  go (n+2) (m+4)
              | otherwise -> ensure 3 $ do
                  poke8 m     $ (w `shiftR` 12) + 0xE0
                  poke8 (m+1) $ ((w `shiftR` 6) .&. 0x3F) + 0x80
                  poke8 (m+2) $ (w .&. 0x3F) + 0x80
                  go (n+1) (m+3)

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

foreign import ccall unsafe "_hs_text_decode_utf8" c_decode_utf8
    :: MutableByteArray# s -> Ptr CSize
    -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "_hs_text_decode_utf8_state" c_decode_utf8_with_state
    :: MutableByteArray# s -> Ptr CSize
    -> Ptr (Ptr Word8) -> Ptr Word8
    -> Ptr CodePoint -> Ptr DecoderState -> IO (Ptr Word8)

foreign import ccall unsafe "_hs_text_decode_latin1" c_decode_latin1
    :: MutableByteArray# s -> Ptr Word8 -> Ptr Word8 -> IO ()
