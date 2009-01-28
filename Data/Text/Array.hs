{-# LANGUAGE CPP, ExistentialQuantification, MagicHash, Rank2Types,
             ScopedTypeVariables, UnboxedTuples #-}
-- |
-- Module      : Data.Text.Array
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com,
-- Stability   : experimental
-- Portability : portable
--
-- Packed, unboxed, heap-resident arrays.  Suitable for performance
-- critical use, both in terms of large data quantities and high
-- speed.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.Text.Array as A
--
-- The names in this module resemble those in the 'Data.Array' family
-- of modules, but are shorter due to the assumption of qualifid
-- naming.
module Data.Text.Array
    (
    -- * Types
      IArray(..)
    , Elt(..)
    , Array
    , MArray

    -- * Functions
    , empty
    , new
    , unsafeNew
    , unsafeFreeze
    , run
    ) where

#if defined(__GLASGOW_HASKELL__)
#include "MachDeps.h"

import GHC.Base (ByteArray#, MutableByteArray#, Int(..), indexWord16Array#,
                 newByteArray#, readWord16Array#, unsafeCoerce#,
                 writeWord16Array#, (+#), (*#))
import GHC.Prim (Int#)
import GHC.ST (ST(..), runST)
import GHC.Word (Word16(..))

#elif defined(__HUGS__)

import Hugs.ByteArray (ByteArray, MutableByteArray, readByteArray,
                       newMutableByteArray, readMutableByteArray,
                       unsafeFreezeMutableByteArray, writeMutableByteArray)
import Foreign.Storable (Storable, sizeOf)
import Hugs.ST (ST(..), runST)

#else
# error not implemented for this compiler
#endif

import Data.Typeable (Typeable1(..), Typeable2(..), TyCon, mkTyCon, mkTyConApp)
import Data.Word (Word16)
import Prelude hiding (length, read)

#include "Typeable.h"

-- | Immutable array type.
data Array e = Array
    {-# UNPACK #-} !Int -- length (in units of e, not bytes)
#if defined(__GLASGOW_HASKELL__)
    ByteArray#
#elif defined(__HUGS__)
    !ByteArray
#endif

INSTANCE_TYPEABLE1(Array,arrayTc,"Array")

-- | Mutable array type, for use in the ST monad.
data MArray s e = MArray
    {-# UNPACK #-} !Int -- length (in units of e, not bytes)
#if defined(__GLASGOW_HASKELL__)
    (MutableByteArray# s)
#elif defined(__HUGS__)
    !(MutableByteArray s)
#endif

INSTANCE_TYPEABLE2(MArray,mArrayTc,"MArray")

-- | Operations supported by all arrays.
class IArray a where
    -- | Return the length of an array.
    length :: a -> Int

instance IArray (Array e) where
    length (Array len _ba) = len
    {-# INLINE length #-}

instance IArray (MArray s e) where
    length (MArray len _ba) = len
    {-# INLINE length #-}

check :: IArray a => String -> a -> Int -> (a -> Int -> b) -> b
check func ary i f
    | i >= 0 && i < length ary = f ary i
    | otherwise = error ("Data.Array.Flat." ++ func ++ ": index out of bounds")
{-# INLINE check #-}

-- | Operations supported by all elements that can be stored in
-- arrays.
class Elt e where
    -- | Indicate how many bytes would be used for an array of the
    -- given size.
    bytesInArray :: Int -> e -> Int
    -- | Unchecked read of an immutable array.  May return garbage or
    -- crash on an out-of-bounds access.
    unsafeIndex :: Array e -> Int -> e
    -- | Unchecked read of a mutable array.  May return garbage or
    -- crash on an out-of-bounds access.
    unsafeRead :: MArray s e -> Int -> ST s e
    -- | Unchecked write of a mutable array.  May return garbage or
    -- crash on an out-of-bounds access.
    unsafeWrite :: MArray s e -> Int -> e -> ST s ()

    -- | Read an immutable array. An invalid index results in a
    -- runtime error.
    index :: Array e -> Int -> e
    index ary i = check "index" ary i unsafeIndex
    {-# INLINE index #-}

    -- | Read a mutable array. An invalid index results in a runtime
    -- error.
    read :: Array e -> Int -> ST s e
    read ary i = check "read" ary i read
    {-# INLINE read #-}

    -- | Write a mutable array. An invalid index results in a runtime
    -- error.
    write :: Array e -> Int -> ST s e
    write ary i = check "write" ary i write
    {-# INLINE write #-}

-- | Freeze a mutable array. Do not mutate the 'MArray' afterwards!
unsafeFreeze :: MArray s e -> ST s (Array e)

#if defined(__GLASGOW_HASKELL__)

wORD16_SCALE :: Int# -> Int#
wORD16_SCALE n# = scale# *# n# where I# scale# = SIZEOF_WORD16

-- | Create an uninitialized mutable array.
unsafeNew :: forall s e. Elt e => Int -> ST s (MArray s e)
unsafeNew n = ST $ \s1# ->
   case bytesInArray n (undefined :: e) of
     (I# len#) -> case newByteArray# len# s1# of
                    (# s2#, marr# #) -> (# s2#, MArray n marr# #)
{-# INLINE unsafeNew #-}

unsafeFreeze (MArray len mba#) = ST $ \s# ->
                                 (# s#, Array len (unsafeCoerce# mba#) #)
{-# INLINE unsafeFreeze #-}

-- | Create a mutable array, with its elements initialized with the
-- given value.
new :: forall s e. Elt e => Int -> e -> ST s (MArray s e)

#elif defined(__HUGS__)

unsafeIndexArray :: Storable e => Array e -> Int -> e
unsafeIndexArray (Array off _len arr) i = readByteArray arr (off + i)

unsafeReadMArray :: Storable e => MArray s e -> Int -> ST s e
unsafeReadMArray (MArray _len marr) = readMutableByteArray marr

unsafeWriteMArray :: Storable e => MArray s e -> Int -> e -> ST s ()
unsafeWriteMArray (MArray _len marr) = writeMutableByteArray marr

-- | Create an uninitialized mutable array.
unsafeNew :: (Storable e) => Int -> ST s (MArray s e)
unsafeNew n = new undefined
  where new :: (Storable e) => e -> ST s (MArray s e)
        new unused = do
          marr <- newMutableByteArray (n * sizeOf unused)
          return (MArray n marr)

unsafeFreeze (MArray len mba) = do
  ba <- unsafeFreezeMutableByteArray mba
  return (Array 0 len ba)

-- | Create a mutable array, with its elements initialized with the
-- given value.
new :: (Storable e) => Int -> e -> ST s (MArray s e)
#endif

new len initVal = do
  marr <- unsafeNew len
  sequence_ [unsafeWrite marr i initVal | i <- [0..len-1]]
  return marr

instance Elt Word16 where
#if defined(__GLASGOW_HASKELL__)

    bytesInArray (I# i#) _ = I# (wORD16_SCALE i#)
    {-# INLINE bytesInArray #-}

    unsafeIndex (Array _len ba#) (I# i#) =
        case indexWord16Array# ba# i# of r# -> (W16# r#)
    {-# INLINE unsafeIndex #-}

    unsafeRead (MArray _len mba#) (I# i#) = ST $ \s# ->
      case readWord16Array# mba# i# s# of
        (# s2#, r# #) -> (# s2#, W16# r# #)
    {-# INLINE unsafeRead #-}

    unsafeWrite (MArray _len marr#) (I# i#) (W16# e#) = ST $ \s1# ->
      case writeWord16Array# marr# i# e# s1# of
        s2# -> (# s2#, () #)
    {-# INLINE unsafeWrite #-}

#elif defined(__HUGS__)

    bytesInArray n w = sizeOf w * n
    unsafeIndex = unsafeIndexArray
    unsafeRead = unsafeReadMArray
    unsafeWrite = unsafeWriteMArray

#endif

-- | An empty immutable array.
empty :: Elt e => Array e
empty = runST (unsafeNew 0 >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result.
run :: Elt e => (forall s. ST s (MArray s e)) -> Array e
run k = runST (k >>= unsafeFreeze)
