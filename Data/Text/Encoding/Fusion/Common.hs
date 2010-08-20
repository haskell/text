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
    , istreamUtf8
    ) where

import Control.Monad.ST
import Data.Bits ((.&.), (.|.))
import Data.Text.Array
import Data.Text.Fusion (Step(..), Stream(..))
import Data.Text.Fusion.Internal (M(..), S(..))
import Data.Text.UnsafeChar (ord, unsafeChr32)
import Data.Text.UnsafeShift (shiftL, shiftR)
import Data.Word (Word8, Word32)
import qualified Data.Text.Encoding.Utf8 as U8

accept, reject :: Word32
accept = 0
reject = 12

data S8 s = S8 !s {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32

istreamUtf8 :: Stream Word8 -> Stream Char
istreamUtf8 (Stream next0 s0 len) =
    Stream next (S8 s0 accept 0) len
  where
    next (S8 s state code) =
      case next0 s of
        Done -> Done
        Skip s' -> Skip (S8 s' state code)
        Yield w s'
          | state' == accept -> Yield (unsafeChr32 code') (S8 s' accept code')
          | state' /= reject -> Skip (S8 s' state' code')
            where
              word = fromIntegral w
              peeku :: Int -> Word32
              peeku n = fromIntegral (unsafeIndexWord8 utf8d n)
              !kind = peeku (fromIntegral word)
              !code' | state /= accept = (word .&. 0x3f) .|. (code `shiftL` 6)
                     | otherwise = (0xff `shiftR` fromIntegral kind) .&. word
              !state' = peeku (256 + fromIntegral (state + kind))
               
                                                           
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

utf8d :: Array
{-# NOINLINE utf8d #-}
utf8d = runST fill where
    fill = do
      ary <- unsafeNew . (`div` 2) . sum . map fst $ xs
      mapM_ (uncurry (unsafeWriteWord8 ary))
            (zip [0..] (concatMap (uncurry replicate) xs))
      unsafeFreeze ary
    xs = [(128,0),(16,1),(16,9),(32,7),(2,8),(30,2),(1,10),(12,3),(1,4),(2,3),
          (1,11),(3,6),(1,5),(11,8),(1,0),(1,12),(1,24),(1,36),(1,60),(1,96),
          (1,84),(3,12),(1,48),(1,72),(13,12),(1,0),(5,12),(1,0),(1,12),(1,0),
          (3,12),(1,24),(5,12),(1,24),(1,12),(1,24),(9,12),(1,24),(5,12),(1,24),
          (7,12),(1,24),(9,12),(1,36),(1,12),(1,36),(3,12),(1,36),(5,12),(1,36),
          (1,12),(1,36),(3,12),(1,36),(10,12)]
