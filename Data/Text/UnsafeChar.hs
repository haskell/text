{-# LANGUAGE MagicHash #-}

-- |
-- Module      : Data.Text.UnsafeChar
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009,
--               (c) Duncan Coutts 2009
--
-- License     : BSD-style
-- Maintainer  : rtharper@aftereternity.co.uk, bos@serpentine.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- Fast character manipulation functions.
module Data.Text.UnsafeChar
    (
      unsafeChr
    , unsafeChr8
    , unsafeChr32
    , unsafeWrite
    ) where

import Control.Monad.ST (ST)
import Data.Bits ((.&.), shiftR)
import Data.Char (ord)
import GHC.Exts (Char(..), chr#, word2Int#)
import GHC.Word (Word8(..), Word16(..), Word32(..))
import qualified Data.Text.Array as A

unsafeChr :: Word16 -> Char
unsafeChr (W16# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr #-}

unsafeChr8 :: Word8 -> Char
unsafeChr8 (W8# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr8 #-}

unsafeChr32 :: Word32 -> Char
unsafeChr32 (W32# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr32 #-}

unsafeWrite :: A.MArray s Word16 -> Int -> Char -> ST s Int
unsafeWrite marr i c
    | n < 0x10000 = do
        A.unsafeWrite marr i (fromIntegral n)
        return (i+1)
    | otherwise = do
        A.unsafeWrite marr i     l
        A.unsafeWrite marr (i+1) r
        return (i+2)
    where n = ord c
          m = n - 0x10000
          l = fromIntegral $ (m `shiftR` 10) + 0xD800
          r = fromIntegral $ (m .&. 0x3FF) + 0xDC00
{-# INLINE unsafeWrite #-}
