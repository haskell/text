{-# LANGUAGE BangPatterns, CPP, RankNTypes, UnboxedTuples #-}

-- |
-- Module      : Data.Text.Internal.Private
-- Copyright   : (c) 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC

module Data.Text.Internal.Private
    (
      runText
    , span_
    , spanAscii_
    ) where

import Control.Monad.ST (ST, runST)
import Data.Text.Internal (Text(..), text)
import Data.Text.Unsafe (Iter(..), iter)
import qualified Data.Text.Array as A
import Data.Word (Word8)

#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

span_ :: (Char -> Bool) -> Text -> (# Text, Text #)
span_ p t@(Text arr off len) = (# hd,tl #)
  where hd = text arr off k
        tl = text arr (off+k) (len-k)
        !k = loop 0
        loop !i | i < len && p c = loop (i+d)
                | otherwise      = i
            where Iter c d       = iter t i
{-# INLINE span_ #-}

-- | For the sake of performance this function does not check
-- that a char is in ASCII range; it is a responsibility of @p@.
--
-- @since 2.0
spanAscii_ :: (Word8 -> Bool) -> Text -> (# Text, Text #)
spanAscii_ p (Text arr off len) = (# hd, tl #)
  where hd = text arr off k
        tl = text arr (off + k) (len - k)
        !k = loop 0
        loop !i | i < len && p (A.unsafeIndex arr (off + i)) = loop (i + 1)
                | otherwise = i
{-# INLINE spanAscii_ #-}

runText ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  (forall s. (A.MArray s -> Int -> ST s Text) -> ST s Text) -> Text
runText act = runST (act $ \ !marr !len -> do
                             A.shrinkM marr len
                             arr <- A.unsafeFreeze marr
                             return $! text arr 0 len)
{-# INLINE runText #-}
