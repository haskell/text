{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.Text.Encoding
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009,
--               (c) Duncan Coutts 2009
--
-- License     : BSD-style
-- Maintainer  : rtharper@aftereternity.co.uk, bos@serpentine.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
--
-- Fusible 'Stream'-oriented functions for converting between 'Text'
-- and several common encodings.

module Data.Text.Encoding.Fusion
    (
    -- * Streaming
      streamASCII
    , streamUtf8
    , streamUtf16LE
    , streamUtf16BE
    , streamUtf32LE
    , streamUtf32BE

    -- * Unstreaming
    , unstream

    -- * Restreaming
    -- Restreaming is the act of converting from one 'Stream'
    -- representation to another.
    , restreamASCII
    , restreamUtf8
    , restreamUtf16LE
    , restreamUtf16BE
    , restreamUtf32LE
    , restreamUtf32BE
    ) where

import Control.Exception (assert)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString as B
import Data.ByteString.Internal (ByteString(..), mallocByteString, memcpy)
import Data.Char (ord)
import Data.Text.Fusion (Step(..), Stream(..))
import Data.Text.UnsafeChar (unsafeChr, unsafeChr8, unsafeChr32)
import Data.Word (Word8, Word16, Word32)
import Foreign.ForeignPtr (withForeignPtr, ForeignPtr)
import Foreign.Storable (pokeByteOff)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text.Utf16 as U16
import qualified Data.Text.Utf32 as U32
import qualified Data.Text.Utf8 as U8

data T4 a b c d = T4 {-# UNPACK #-} !a {-# UNPACK #-} !b {-# UNPACK #-} !c {-# UNPACK #-} !d

streamASCII :: ByteString -> Stream Char
streamASCII bs = Stream next 0 l
    where
      l = B.length bs
      {-# INLINE next #-}
      next i
          | i >= l    = Done
          | otherwise = Yield (unsafeChr8 x1) (i+1)
          where
            x1 = B.unsafeIndex bs i
{-# INLINE [0] streamASCII #-}

-- | /O(n) Convert a ByteString into a Stream Char, using the specified encoding standard.
streamUtf8 :: ByteString -> Stream Char
streamUtf8 bs = Stream next 0 l
    where
      l = B.length bs
      {-# INLINE next #-}
      next i
          | i >= l = Done
          | U8.validate1 x1 = Yield (unsafeChr8 x1) (i+1)
          | i+1 < l && U8.validate2 x1 x2 = Yield (U8.chr2 x1 x2) (i+2)
          | i+2 < l && U8.validate3 x1 x2 x3 = Yield (U8.chr3 x1 x2 x3) (i+3)
          | i+3 < l && U8.validate4 x1 x2 x3 x4 = Yield (U8.chr4 x1 x2 x3 x4) (i+4)
          | otherwise = encodingError "UTF-8"
          where
            x1 = idx i
            x2 = idx (i + 1)
            x3 = idx (i + 2)
            x4 = idx (i + 3)
            idx = B.unsafeIndex bs
{-# INLINE [0] streamUtf8 #-}

streamUtf16LE :: ByteString -> Stream Char
streamUtf16LE bs = Stream next 0 l
    where
      l = B.length bs
      {-# INLINE next #-}
      next i
          | i >= l                         = Done
          | i+1 < l && U16.validate1 x1    = Yield (unsafeChr x1) (i+2)
          | i+3 < l && U16.validate2 x1 x2 = Yield (U16.chr2 x1 x2) (i+4)
          | otherwise = encodingError "UTF-16LE"
          where
            x1    = (shiftL (idx (i + 1)) 8) + (idx i)
            x2    = (shiftL (idx (i + 3)) 8) + (idx (i + 2))
            idx = fromIntegral . B.unsafeIndex bs :: Int -> Word16
{-# INLINE [0] streamUtf16LE #-}

streamUtf16BE :: ByteString -> Stream Char
streamUtf16BE bs = Stream next 0 l
    where
      l = B.length bs
      {-# INLINE next #-}
      next i
          | i >= l                         = Done
          | i+1 < l && U16.validate1 x1    = Yield (unsafeChr x1) (i+2)
          | i+3 < l && U16.validate2 x1 x2 = Yield (U16.chr2 x1 x2) (i+4)
          | otherwise = encodingError "UTF16-BE"
          where
            x1    = (shiftL (idx i) 8) + (idx (i + 1))
            x2    = (shiftL (idx (i + 2)) 8) + (idx (i + 3))
            idx = fromIntegral . B.unsafeIndex bs :: Int -> Word16
{-# INLINE [0] streamUtf16BE #-}

streamUtf32BE :: ByteString -> Stream Char
streamUtf32BE bs = Stream next 0 l
    where
      l = B.length bs
      {-# INLINE next #-}
      next i
          | i >= l                    = Done
          | i+3 < l && U32.validate x = Yield (unsafeChr32 x) (i+4)
          | otherwise                 = encodingError "UTF-32BE"
          where
            x     = shiftL x1 24 + shiftL x2 16 + shiftL x3 8 + x4
            x1    = idx i
            x2    = idx (i+1)
            x3    = idx (i+2)
            x4    = idx (i+3)
            idx = fromIntegral . B.unsafeIndex bs :: Int -> Word32
{-# INLINE [0] streamUtf32BE #-}

streamUtf32LE :: ByteString -> Stream Char
streamUtf32LE bs = Stream next 0 l
    where
      l = B.length bs
      {-# INLINE next #-}
      next i
          | i >= l                    = Done
          | i+3 < l && U32.validate x = Yield (unsafeChr32 x) (i+4)
          | otherwise                 = encodingError "UTF-32LE"
          where
            x     = shiftL x4 24 + shiftL x3 16 + shiftL x2 8 + x1
            x1    = idx i
            x2    = idx $ i+1
            x3    = idx $ i+2
            x4    = idx $ i+3
            idx = fromIntegral . B.unsafeIndex bs :: Int -> Word32
{-# INLINE [0] streamUtf32LE #-}

restreamASCII :: Stream Char -> Stream Word8
restreamASCII (Stream next0 s0 len) =  Stream next s0 (len*2)
    where
      next !s = case next0 s of
                  Done -> Done
                  Skip s' -> Skip s'
                  Yield x xs -> Yield x' xs
                      where x' = fromIntegral (ord x) :: Word8
{-# INLINE restreamASCII #-}

data M = N | J {-# UNPACK #-} !Word8
       deriving (Eq, Ord, Show)

-- | /O(n)/ Convert a Stream Char into a UTF-8 encoded Stream Word8.
restreamUtf8 :: Stream Char -> Stream Word8
restreamUtf8 (Stream next0 s0 len) =
    Stream next (T4 s0 N N N) (len*2)
    where
      {-# INLINE next #-}
      next (T4 s N N N) = case next0 s of
                  Done              -> Done
                  Skip s'           -> Skip (T4 s' N N N)
                  Yield x xs
                      | n <= 0x7F   -> Yield c  (T4 xs N N N)
                      | n <= 0x07FF -> Yield a2 (T4 xs (J b2) N N)
                      | n <= 0xFFFF -> Yield a3 (T4 xs (J b3) (J c3) N)
                      | otherwise   -> Yield a4 (T4 xs (J b4) (J c4) (J d4))
                      where
                        n  = ord x
                        c  = fromIntegral n
                        (a2,b2) = U8.ord2 x
                        (a3,b3,c3) = U8.ord3 x
                        (a4,b4,c4,d4) = U8.ord4 x
      next (T4 s (J x2) N N)   = Yield x2 (T4 s N N N)
      next (T4 s (J x2) x3 N)  = Yield x2 (T4 s x3 N N)
      next (T4 s (J x2) x3 x4) = Yield x2 (T4 s x3 x4 N)
      next _ = internalError "restreamUtf8"
{-# INLINE restreamUtf8 #-}

restreamUtf16BE :: Stream Char -> Stream Word8
restreamUtf16BE (Stream next0 s0 len) =
    Stream next (T4 (Just s0) Nothing Nothing Nothing) (len*2)
    where
      {-# INLINE next #-}
      next (T4 (Just s) Nothing Nothing Nothing) = case next0 s of
          Done -> Done
          Skip s' -> Skip (T4 (Just s') Nothing Nothing Nothing)
          Yield x xs
              | n < 0x10000 -> Yield (fromIntegral $ shiftR n 8) (T4 (Just xs) (Just (fromIntegral n)) Nothing Nothing)
              | otherwise   -> Yield c1                          (T4 (Just xs) (Just c2) (Just c3) (Just c4))
              where
                n  = ord x
                n1 = n - 0x10000
                c1 = fromIntegral (shiftR n1 18 + 0xD8)
                c2 = fromIntegral (shiftR n1 10)
                n2 = n1 .&. 0x3FF
                c3 = fromIntegral (shiftR n2 8 + 0xDC)
                c4 = fromIntegral n2
      next (T4 (Just s) (Just x2) Nothing Nothing) = Yield x2 (T4 (Just s) Nothing Nothing Nothing)
      next (T4 (Just s) (Just x2) x3 Nothing)      = Yield x2 (T4 (Just s) x3 Nothing Nothing)
      next (T4 (Just s) (Just x2) x3 x4)           = Yield x2 (T4 (Just s) x3 x4 Nothing)
      next _ = internalError "restreamUtf16BE"
{-# INLINE restreamUtf16BE #-}

restreamUtf16LE :: Stream Char -> Stream Word8
restreamUtf16LE (Stream next0 s0 len) =
    Stream next (T4 (Just s0) Nothing Nothing Nothing) (len*2)
    where
      {-# INLINE next #-}
      next (T4 (Just s) Nothing Nothing Nothing) = case next0 s of
          Done -> Done
          Skip s' -> Skip (T4 (Just s') Nothing Nothing Nothing)
          Yield x xs
              | n < 0x10000 -> Yield (fromIntegral n) (T4 (Just xs) (Just (fromIntegral $ shiftR n 8)) Nothing Nothing)
              | otherwise   -> Yield c1                          (T4 (Just xs) (Just c2) (Just c3) (Just c4))
              where
                n  = ord x
                n1 = n - 0x10000
                c2 = fromIntegral (shiftR n1 18 + 0xD8)
                c1 = fromIntegral (shiftR n1 10)
                n2 = n1 .&. 0x3FF
                c4 = fromIntegral (shiftR n2 8 + 0xDC)
                c3 = fromIntegral n2
      next (T4 (Just s) (Just x2) Nothing Nothing) = Yield x2 (T4 (Just s) Nothing Nothing Nothing)
      next (T4 (Just s) (Just x2) x3 Nothing)      = Yield x2 (T4 (Just s) x3 Nothing Nothing)
      next (T4 (Just s) (Just x2) x3 x4)           = Yield x2 (T4 (Just s) x3 x4 Nothing)
      next _ = internalError "restreamUtf16LE"
{-# INLINE restreamUtf16LE #-}

restreamUtf32BE :: Stream Char -> Stream Word8
restreamUtf32BE (Stream next0 s0 len) =
    Stream next (T4 (Just s0) Nothing Nothing Nothing) (len*2)
    where
    {-# INLINE next #-}
    next (T4 (Just s) Nothing Nothing Nothing) = case next0 s of
        Done       -> Done
        Skip s'    -> Skip (T4 (Just s') Nothing Nothing Nothing)
        Yield x xs -> Yield c1 (T4 (Just xs) (Just c2) (Just c3) (Just c4))
          where
            n  = ord x
            c1 = fromIntegral $ shiftR n 24
            c2 = fromIntegral $ shiftR n 16
            c3 = fromIntegral $ shiftR n 8
            c4 = fromIntegral n
    next (T4 (Just s) (Just x2) Nothing Nothing) = Yield x2 (T4 (Just s) Nothing Nothing Nothing)
    next (T4 (Just s) (Just x2) x3 Nothing)      = Yield x2 (T4 (Just s) x3 Nothing Nothing)
    next (T4 (Just s) (Just x2) x3 x4)           = Yield x2 (T4 (Just s) x3 x4 Nothing)
    next _ = internalError "restreamUtf32BE"
{-# INLINE restreamUtf32BE #-}

restreamUtf32LE :: Stream Char -> Stream Word8
restreamUtf32LE (Stream next0 s0 len) =
    Stream next (T4 (Just s0) Nothing Nothing Nothing) (len*2)
    where
    {-# INLINE next #-}
    next (T4 (Just s) Nothing Nothing Nothing) = case next0 s of
        Done       -> Done
        Skip s'    -> Skip (T4 (Just s') Nothing Nothing Nothing)
        Yield x xs -> Yield c1 (T4 (Just xs) (Just c2) (Just c3) (Just c4))
          where
            n  = ord x
            c4 = fromIntegral $ shiftR n 24
            c3 = fromIntegral $ shiftR n 16
            c2 = fromIntegral $ shiftR n 8
            c1 = fromIntegral n
    next (T4 (Just s) (Just x2) Nothing Nothing) = Yield x2 (T4 (Just s) Nothing Nothing Nothing)
    next (T4 (Just s) (Just x2) x3 Nothing)      = Yield x2 (T4 (Just s) x3 Nothing Nothing)
    next (T4 (Just s) (Just x2) x3 x4)           = Yield x2 (T4 (Just s) x3 x4 Nothing)
    next _ = internalError "restreamUtf32LE"
{-# INLINE restreamUtf32LE #-}


-- | /O(n)/ Convert a 'Stream' 'Word8' to a 'ByteString'.
unstream :: Stream Word8 -> ByteString
unstream (Stream next s0 len) = unsafePerformIO $ do
    fp0 <- mallocByteString len
    loop fp0 len 0 s0
    where
      loop !fp !n !off !s = case next s of
          Done -> trimUp fp n off
          Skip s' -> loop fp n off s'
          Yield x s'
              | n == off -> realloc fp n off s' x
              | otherwise -> do
            withForeignPtr fp $ \p -> pokeByteOff p off x
            loop fp n (off+1) s'
      {-# NOINLINE realloc #-}
      realloc fp n off s x = do
        let n' = n+n
        fp' <- copy0 fp n n'
        withForeignPtr fp' $ \p -> pokeByteOff p off x
        loop fp' n' (off+1) s
      {-# NOINLINE trimUp #-}
      trimUp fp _ off = return $! PS fp 0 off
      copy0 :: ForeignPtr Word8 -> Int -> Int -> IO (ForeignPtr Word8)
      copy0 !src !srcLen !destLen = assert (srcLen <= destLen) $ do
          dest <- mallocByteString destLen
          withForeignPtr src  $ \src'  ->
              withForeignPtr dest $ \dest' ->
                  memcpy dest' src' (fromIntegral destLen)
          return dest

internalError :: String -> a
internalError func =
    error $ "Data.Text.Encoding.Fusion." ++ func ++ ": internal error"

encodingError :: String -> a
encodingError encoding =
    error $ "Data.Text.Encoding.Fusion: Bad " ++ encoding ++ " stream"
