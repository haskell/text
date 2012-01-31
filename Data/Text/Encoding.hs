{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, MagicHash,
    UnliftedFFITypes #-}
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

    -- * Encoding Text to ByteStrings
    , encodeUtf8
    , encodeUtf16LE
    , encodeUtf16BE
    , encodeUtf32LE
    , encodeUtf32BE
    
    -- * Generic encoding of Text
    , encodeStreamWithB
    , encodeTextWithB
    , encodeUtf8Builder
    , encodeUtf8Escaped
    ) where

import Control.Exception (evaluate, try)
#if __GLASGOW_HASKELL__ >= 702
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
#else
import Control.Monad.ST (unsafeIOToST, unsafeSTToIO)
#endif
import Data.Bits ((.&.))
import Data.ByteString as B
import Data.ByteString.Internal as B
import Data.ByteString.Lazy.Builder.Internal as B
import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Internal as B
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException, strictDecode)
import Data.Text.Internal (Text(..), textP)
import Data.Text.UnsafeChar (ord, unsafeWrite)
import Data.Text.UnsafeShift (shiftL, shiftR)
import Data.Word (Word8)
import Foreign.C.Types (CSize)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (peek, poke)
import GHC.Base (MutableByteArray#)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.Array as A
import qualified Data.Text.Encoding.Fusion as E
import qualified Data.Text.Encoding.Utf16 as U16
import qualified Data.Text.Fusion as F

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
-- This function is deprecated.  Use 'decodeUtf8' instead.
decodeASCII :: ByteString -> Text
decodeASCII = decodeUtf8
{-# DEPRECATED decodeASCII "Use decodeUtf8 instead" #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
decodeUtf8With :: OnDecodeError -> ByteString -> Text
decodeUtf8With onErr (PS fp off len) = textP (fst a) 0 (snd a)
 where
  a = A.run2 (A.new len >>= unsafeIOToST . go)
  desc = "Data.Text.Encoding.decodeUtf8: Invalid UTF-8 stream"
  go dest = withForeignPtr fp $ \ptr ->
    with (0::CSize) $ \destOffPtr -> do
      let end = ptr `plusPtr` (off + len)
          loop curPtr = do
            curPtr' <- c_decode_utf8 (A.maBA dest) destOffPtr curPtr end
            if curPtr' == end
              then do
                n <- peek destOffPtr
                return (dest,fromIntegral n)
              else do
                x <- peek curPtr'
                case onErr desc (Just x) of
                  Nothing -> loop $ curPtr' `plusPtr` 1
                  Just c -> do
                    destOff <- peek destOffPtr
                    w <- unsafeSTToIO $
                         unsafeWrite dest (fromIntegral destOff) c
                    poke destOffPtr (destOff + fromIntegral w)
                    loop $ curPtr' `plusPtr` 1
      loop (ptr `plusPtr` off)
{- INLINE[0] decodeUtf8With #-}

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

-- | Decode a 'ByteString' containing UTF-8 encoded text..
--
-- If the input contains any invalid UTF-8 data, the relevant
-- exception will be returned, otherwise the decoded text.
decodeUtf8' :: ByteString -> Either UnicodeException Text
decodeUtf8' = unsafePerformIO . try . evaluate . decodeUtf8With strictDecode
{-# INLINE decodeUtf8' #-}

-- | Encode text using UTF-8 encoding.
encodeUtf8 :: Text -> ByteString
encodeUtf8 (Text arr off len) = unsafePerformIO $ do
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

-- | Encode all elements of a 'F.Stream' using a 'B.BoundedEncoding'.
{-# INLINE encodeStreamWithB #-}
encodeStreamWithB :: B.BoundedEncoding a -> F.Stream a -> B.Builder
encodeStreamWithB be = 
    \(F.Stream next s0 _) -> B.builder $ step next s0
  where
    bound = B.sizeBound be
    step next s0 k (B.BufferRange op0 ope0) = 
        go s0 op0
      where
        go s !op = case next s of
          F.Done       -> k (B.BufferRange op ope0)
          F.Skip s'    -> go s' op
          F.Yield x s'
            | op `plusPtr` bound <= ope0 -> B.runB be x op >>= go s'
            | otherwise                  -> 
                return $ B.bufferFull bound op (step next s k)


-- | 
-- | /Subject to fusion./
-- Encode all 'Char's of a 'T.Text' using a 'B.BoundedEncoding'.
{-# INLINE encodeTextWithB #-}
encodeTextWithB :: B.BoundedEncoding Char -> Text -> B.Builder
encodeTextWithB be = encodeStreamWithB be . F.stream

-- | Encode text using UTF-8 encoding.
encodeUtf8Builder :: Text -> B.Builder
encodeUtf8Builder (Text arr off len) =
    B.builder step
  where
    bound   = 4
    iend    = off + len
    step !k =
        outerLoop off
      where
        outerLoop !i0 !br@(BufferRange op0 ope)
          | i0 >= iend                = k br
          | op0 `plusPtr` bound < ope =
              goPartial (i0 + min outRemaining inpRemaining)
          | otherwise  = return $ bufferFull bound op0 (outerLoop i0)
          where
            outRemaining = (ope `minusPtr` op0) `div` bound
            inpRemaining = iend - i0

            goPartial !iendTmp = go i0 op0
              where
                go !i !op
                  | i < iendTmp = case A.unsafeIndex arr i of
                      w | w <= 0x7F -> do
                            poke8 0 w
                            go (i + 1) (op `plusPtr` 1)
                        | w <= 0x7FF -> do
                            poke8 0 $ (w `shiftR` 6) + 0xC0
                            poke8 1 $ (w .&. 0x3f) + 0x80
                            go (i + 1) (op `plusPtr` 2)
                        | 0xD800 <= w && w <= 0xDBFF -> do
                            let c = ord $ U16.chr2 w (A.unsafeIndex arr (i+1))
                            poke8 0 $ (c `shiftR` 18) + 0xF0
                            poke8 1 $ ((c `shiftR` 12) .&. 0x3F) + 0x80
                            poke8 2 $ ((c `shiftR` 6) .&. 0x3F) + 0x80
                            poke8 3 $ (c .&. 0x3F) + 0x80
                            go (i + 2) (op `plusPtr` 4)
                        | otherwise -> do
                            poke8 0 $ (w `shiftR` 12) + 0xE0
                            poke8 1 $ ((w `shiftR` 6) .&. 0x3F) + 0x80
                            poke8 2 $ (w .&. 0x3F) + 0x80
                            go (i + 1) (op `plusPtr` 3)
                  | otherwise =
                      outerLoop i (BufferRange op ope)
                  where
                    poke8 j v = poke (op `plusPtr` j) (fromIntegral v :: Word8)

-- | Encode text using UTF-8 encoding and escape the ASCII characters using
-- a 'BoundedEncoding'.
encodeUtf8Escaped :: B.BoundedEncoding Word8 -> Text -> B.Builder
encodeUtf8Escaped be (Text arr off len) =
    B.builder step
  where
    bound   = max 4 $ B.sizeBound be
    iend    = off + len
    step !k =
        outerLoop off
      where
        outerLoop !i0 !br@(BufferRange op0 ope)
          | i0 >= iend                = k br
          | op0 `plusPtr` bound < ope =
              goPartial (i0 + min outRemaining inpRemaining)
          | otherwise  = return $ bufferFull bound op0 (outerLoop i0)
          where
            outRemaining = (ope `minusPtr` op0) `div` bound
            inpRemaining = iend - i0

            goPartial !iendTmp = go i0 op0
              where
                go !i !op
                  | i < iendTmp = case A.unsafeIndex arr i of
                      w | w <= 0x7F -> do
                            B.runB be (fromIntegral w) op >>= go (i + 1)
                        | w <= 0x7FF -> do
                            poke8 0 $ (w `shiftR` 6) + 0xC0
                            poke8 1 $ (w .&. 0x3f) + 0x80
                            go (i + 1) (op `plusPtr` 2)
                        | 0xD800 <= w && w <= 0xDBFF -> do
                            let c = ord $ U16.chr2 w (A.unsafeIndex arr (i+1))
                            poke8 0 $ (c `shiftR` 18) + 0xF0
                            poke8 1 $ ((c `shiftR` 12) .&. 0x3F) + 0x80
                            poke8 2 $ ((c `shiftR` 6) .&. 0x3F) + 0x80
                            poke8 3 $ (c .&. 0x3F) + 0x80
                            go (i + 2) (op `plusPtr` 4)
                        | otherwise -> do
                            poke8 0 $ (w `shiftR` 12) + 0xE0
                            poke8 1 $ ((w `shiftR` 6) .&. 0x3F) + 0x80
                            poke8 2 $ (w .&. 0x3F) + 0x80
                            go (i + 1) (op `plusPtr` 3)
                  | otherwise =
                      outerLoop i (BufferRange op ope)
                  where
                    poke8 j v = poke (op `plusPtr` j) (fromIntegral v :: Word8)

