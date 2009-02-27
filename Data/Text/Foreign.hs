{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Text.Foreign
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : rtharper@aftereternity.co.uk, bos@serpentine.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- Support for using 'Text' data with native code via the Haskell
-- foreign function interface.

module Data.Text.Foreign
    (
    -- * Interoperability with native code
    -- $interop

    -- * Safe conversion functions
      fromPtr
    , useAsPtr
    -- * Unsafe conversion code
    , lengthWord16
    , unsafeCopyToPtr
    ) where

import Control.Exception (assert)
import Control.Monad.ST (unsafeIOToST)
import Data.Text.Internal (Text(..), empty)
import qualified Data.Text.Array as A
import Data.Word (Word16)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, poke)

-- $interop
--
-- The 'Text' type is implemented using arrays that are not guaranteed
-- to have a fixed address in the Haskell heap. All communication with
-- native code must thus occur by copying data back and forth.
--
-- The 'Text' type's internal representation is UTF-16, using the
-- platform's native endianness.  This makes copied data suitable for
-- use with native libraries that use a similar representation, such
-- as ICU.  To interoperate with native libraries that use different
-- internal representations, such as UTF-8 or UTF-32, consider using
-- the functions in the 'Data.Text.Encoding' module.

-- | /O(n)/ Create a new 'Text' from a 'Ptr' 'Word16' by copying the
-- contents of the array.
fromPtr :: Ptr Word16           -- ^ source array
        -> Int                  -- ^ length of source array (in 'Word16' units)
        -> IO Text
fromPtr _   0   = return empty
fromPtr ptr len = assert (len > 0) $ return (Text arr 0 len)
  where
    arr = A.run (A.unsafeNew len >>= copy)
    copy marr = loop ptr 0
      where
        loop !p !i | i == len = return marr
                   | otherwise = do
          A.unsafeWrite marr i =<< unsafeIOToST (peek p)
          loop (p `plusPtr` 2) (i + 1)

-- | /O(1)/ Return the length of a 'Text' in units of 'Word16'.  This
-- is useful for sizing a target array appropriately before using
-- 'unsafeCopyToPtr'.
lengthWord16 :: Text -> Int
lengthWord16 (Text _arr _off len) = len

-- | /O(n)/ Copy a 'Text' to an array.  The array is assumed to be big
-- enough to hold the contents of the entire 'Text'.
unsafeCopyToPtr :: Text -> Ptr Word16 -> IO ()
unsafeCopyToPtr (Text arr off len) ptr = loop ptr off
  where
    end = off + len
    loop !p !i | i == end  = return ()
               | otherwise = do
      poke p (A.unsafeIndex arr i)
      loop (p `plusPtr` 2) (i + 1)

-- | /O(n)/ Perform an action on a temporary, mutable copy of a
-- 'Text'.  The copy is freed as soon as the action returns.
useAsPtr :: Text -> (Ptr Word16 -> Int -> IO a) -> IO a
useAsPtr t@(Text _arr _off len) action =
    allocaBytes (len * 2) $ \buf -> do
      unsafeCopyToPtr t buf
      action (castPtr buf) len
