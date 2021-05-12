{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Unsafe.Shift
-- Copyright   : (c) Bryan O'Sullivan 2009
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
-- Fast, unchecked bit shifting functions.

module Data.Text.Internal.Unsafe.Shift
    (
      UnsafeShift(..)
    ) where

import qualified Data.Bits as Bits
import Data.Word

-- | This is a workaround for poor optimisation in GHC 6.8.2.  It
-- fails to notice constant-width shifts, and adds a test and branch
-- to every shift.  This imposes about a 10% performance hit.
--
-- These functions are undefined when the amount being shifted by is
-- greater than the size in bits of a machine Int#.
class UnsafeShift a where
    shiftL :: a -> Int -> a
    {-# INLINE shiftL #-}
    default shiftL :: Bits.Bits a => a -> Int -> a
    shiftL = Bits.unsafeShiftL

    shiftR :: a -> Int -> a
    {-# INLINE shiftR #-}
    default shiftR :: Bits.Bits a => a -> Int -> a
    shiftR = Bits.unsafeShiftR

instance UnsafeShift Word16 where
instance UnsafeShift Word32 where
instance UnsafeShift Word64 where
instance UnsafeShift Int where
