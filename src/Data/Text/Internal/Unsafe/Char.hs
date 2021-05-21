{-# LANGUAGE CPP, MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Unsafe.Char
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Fast character manipulation functions.
module Data.Text.Internal.Unsafe.Char
    (
      ord
    , unsafeChr
    , unsafeChr8
    , unsafeChr32
    , unsafeWrite
    ) where

import Control.Monad.ST (ST)
import Data.Bits ((.&.))
import Data.Text.Internal.Unsafe.Shift (shiftR)
import GHC.Exts (Char(..), Int(..), chr#, ord#, word2Int#)
import GHC.Word (Word8(..), Word16(..), Word32(..))
import qualified Data.Text.Array as A
import Data.Text.Internal.PrimCompat ( word8ToWord#, word16ToWord#, word32ToWord# )
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
{-# INLINE ord #-}

unsafeChr :: Word16 -> Char
unsafeChr (W16# w#) = C# (chr# (word2Int# (word16ToWord# w#)))
{-# INLINE unsafeChr #-}

unsafeChr8 :: Word8 -> Char
unsafeChr8 (W8# w#) = C# (chr# (word2Int# (word8ToWord# w#)))
{-# INLINE unsafeChr8 #-}

unsafeChr32 :: Word32 -> Char
unsafeChr32 (W32# w#) = C# (chr# (word2Int# (word32ToWord# w#)))
{-# INLINE unsafeChr32 #-}

-- | Write a character into the array at the given offset.  Returns
-- the number of 'Word16's written.
unsafeWrite ::
#if defined(ASSERTS)
    HasCallStack =>
#endif
    A.MArray s -> Int -> Char -> ST s Int
unsafeWrite marr i c
    | n < 0x10000 = do
        A.unsafeWrite marr i (intToWord16 n)
        return 1
    | otherwise = do
        A.unsafeWrite marr i lo
        A.unsafeWrite marr (i+1) hi
        return 2
    where n = ord c
          m = n - 0x10000
          lo = intToWord16 $ (m `shiftR` 10) + 0xD800
          hi = intToWord16 $ (m .&. 0x3FF) + 0xDC00
{-# INLINE unsafeWrite #-}

intToWord16 :: Int -> Word16
intToWord16 = fromIntegral
