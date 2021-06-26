{-# LANGUAGE CPP #-}

-- |
-- Module      : Data.Text.Internal.Functions
-- Copyright   : 2010 Bryan O'Sullivan
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
-- Useful functions.

module Data.Text.Internal.Functions
    (
      unsafeWithForeignPtr
    ) where

import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr)
#if MIN_VERSION_base(4,15,0)
import qualified GHC.ForeignPtr (unsafeWithForeignPtr)
#else
import qualified Foreign.ForeignPtr (withForeignPtr)
#endif

unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
#if MIN_VERSION_base(4,15,0)
unsafeWithForeignPtr = GHC.ForeignPtr.unsafeWithForeignPtr
#else
unsafeWithForeignPtr = Foreign.ForeignPtr.withForeignPtr
#endif
