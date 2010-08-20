{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.Text.Encoding.Fusion.Common
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009,
--               (c) Duncan Coutts 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
--
-- Fusible 'Stream'-oriented functions for converting between 'Text'
-- and several common encodings.

module Data.Text.Encoding.Fusion.Common
    (
    -- * Restreaming
    -- Restreaming is the act of converting from one 'Stream'
    -- representation to another.
      restreamUtf8
    , restreamUtf16LE
    , restreamUtf16BE
    , restreamUtf32LE
    , restreamUtf32BE
    ) where

import Data.Bits ((.&.))
import Data.Text.Fusion (Step(..), Stream(..))
import Data.Text.Fusion.Internal (M(..), S(..))
import Data.Text.UnsafeChar (ord)
import Data.Text.UnsafeShift (shiftR)
import Data.Word (Word8)
import qualified Data.Text.Encoding.Utf8 as U8

-- | /O(n)/ Convert a Stream Char into a UTF-8 encoded Stream Word8.
restreamUtf8 :: Stream Char -> Stream Word8
restreamUtf8 (Stream next0 s0 len) =
    Stream next (S s0 N N N) (len*2)
    where
      {-# INLINE next #-}
      next (S s N N N) = case next0 s of
                  Done              -> Done
                  Skip s'           -> Skip (S s' N N N)
                  Yield x xs
                      | n <= 0x7F   -> Yield c  (S xs N N N)
                      | n <= 0x07FF -> Yield a2 (S xs (J b2) N N)
                      | n <= 0xFFFF -> Yield a3 (S xs (J b3) (J c3) N)
                      | otherwise   -> Yield a4 (S xs (J b4) (J c4) (J d4))
                      where
                        n  = ord x
                        c  = fromIntegral n
                        (a2,b2) = U8.ord2 x
                        (a3,b3,c3) = U8.ord3 x
                        (a4,b4,c4,d4) = U8.ord4 x
      next (S s (J x2) N N)   = Yield x2 (S s N N N)
      next (S s (J x2) x3 N)  = Yield x2 (S s x3 N N)
      next (S s (J x2) x3 x4) = Yield x2 (S s x3 x4 N)
      next _ = internalError "restreamUtf8"
{-# INLINE restreamUtf8 #-}

restreamUtf16BE :: Stream Char -> Stream Word8
restreamUtf16BE (Stream next0 s0 len) =
    Stream next (S s0 N N N) (len*2)
    where
      {-# INLINE next #-}
      next (S s N N N) = case next0 s of
          Done -> Done
          Skip s' -> Skip (S s' N N N)
          Yield x xs
              | n < 0x10000 -> Yield (fromIntegral $ n `shiftR` 8) $
                               S xs (J $ fromIntegral n) N N
              | otherwise   -> Yield c1 $
                               S xs (J c2) (J c3) (J c4)
              where
                n  = ord x
                n1 = n - 0x10000
                c1 = fromIntegral (n1 `shiftR` 18 + 0xD8)
                c2 = fromIntegral (n1 `shiftR` 10)
                n2 = n1 .&. 0x3FF
                c3 = fromIntegral (n2 `shiftR` 8 + 0xDC)
                c4 = fromIntegral n2
      next (S s (J x2) N N)   = Yield x2 (S s N N N)
      next (S s (J x2) x3 N)  = Yield x2 (S s x3 N N)
      next (S s (J x2) x3 x4) = Yield x2 (S s x3 x4 N)
      next _ = internalError "restreamUtf16BE"
{-# INLINE restreamUtf16BE #-}

restreamUtf16LE :: Stream Char -> Stream Word8
restreamUtf16LE (Stream next0 s0 len) =
    Stream next (S s0 N N N) (len*2)
    where
      {-# INLINE next #-}
      next (S s N N N) = case next0 s of
          Done -> Done
          Skip s' -> Skip (S s' N N N)
          Yield x xs
              | n < 0x10000 -> Yield (fromIntegral n) $
                               S xs (J (fromIntegral $ shiftR n 8)) N N
              | otherwise   -> Yield c1 $
                               S xs (J c2) (J c3) (J c4)
              where
                n  = ord x
                n1 = n - 0x10000
                c2 = fromIntegral (shiftR n1 18 + 0xD8)
                c1 = fromIntegral (shiftR n1 10)
                n2 = n1 .&. 0x3FF
                c4 = fromIntegral (shiftR n2 8 + 0xDC)
                c3 = fromIntegral n2
      next (S s (J x2) N N)   = Yield x2 (S s N N N)
      next (S s (J x2) x3 N)  = Yield x2 (S s x3 N N)
      next (S s (J x2) x3 x4) = Yield x2 (S s x3 x4 N)
      next _ = internalError "restreamUtf16LE"
{-# INLINE restreamUtf16LE #-}

restreamUtf32BE :: Stream Char -> Stream Word8
restreamUtf32BE (Stream next0 s0 len) =
    Stream next (S s0 N N N) (len*2)
    where
    {-# INLINE next #-}
    next (S s N N N) = case next0 s of
        Done       -> Done
        Skip s'    -> Skip (S s' N N N)
        Yield x xs -> Yield c1 (S xs (J c2) (J c3) (J c4))
          where
            n  = ord x
            c1 = fromIntegral $ shiftR n 24
            c2 = fromIntegral $ shiftR n 16
            c3 = fromIntegral $ shiftR n 8
            c4 = fromIntegral n
    next (S s (J x2) N N) = Yield x2 (S s N N N)
    next (S s (J x2) x3 N)      = Yield x2 (S s x3 N N)
    next (S s (J x2) x3 x4)           = Yield x2 (S s x3 x4 N)
    next _ = internalError "restreamUtf32BE"
{-# INLINE restreamUtf32BE #-}

restreamUtf32LE :: Stream Char -> Stream Word8
restreamUtf32LE (Stream next0 s0 len) =
    Stream next (S s0 N N N) (len*2)
    where
    {-# INLINE next #-}
    next (S s N N N) = case next0 s of
        Done       -> Done
        Skip s'    -> Skip (S s' N N N)
        Yield x xs -> Yield c1 (S xs (J c2) (J c3) (J c4))
          where
            n  = ord x
            c4 = fromIntegral $ shiftR n 24
            c3 = fromIntegral $ shiftR n 16
            c2 = fromIntegral $ shiftR n 8
            c1 = fromIntegral n
    next (S s (J x2) N N)   = Yield x2 (S s N N N)
    next (S s (J x2) x3 N)  = Yield x2 (S s x3 N N)
    next (S s (J x2) x3 x4) = Yield x2 (S s x3 x4 N)
    next _ = internalError "restreamUtf32LE"
{-# INLINE restreamUtf32LE #-}

internalError :: String -> a
internalError func =
    error $ "Data.Text.Encoding.Fusion.Common." ++ func ++ ": internal error"
