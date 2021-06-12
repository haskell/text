{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types,
    RecordWildCards, UnboxedTuples, UnliftedFFITypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- |
-- Module      : Data.Text.Array
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
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
-- of modules, but are shorter due to the assumption of qualified
-- naming.
module Data.Text.Array
    (
    -- * Types
      Array(..)
    , MArray(..)
    -- * Functions
    , copyM
    , copyI
    , empty
    , equal
    , run
    , run2
    , toList
    , unsafeFreeze
    , unsafeIndex
    , new
    , unsafeWrite
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
import GHC.Stack (HasCallStack)
#endif
import Data.Bits ((.&.), xor, shiftL, shiftR)
#if !MIN_VERSION_base(4,11,0)
import Data.Text.Internal.Unsafe (inlinePerformIO)
#endif
import GHC.Exts hiding (toList)
import GHC.ST (ST(..), runST)
import GHC.Word (Word16(..))
import Prelude hiding (length, read)

-- | Immutable array type.
--
-- The 'Array' constructor is exposed since @text-1.1.1.3@
data Array = Array { aBA :: ByteArray# }

-- | Mutable array type, for use in the ST monad.
--
-- The 'MArray' constructor is exposed since @text-1.1.1.3@
data MArray s = MArray { maBA :: MutableByteArray# s }

-- | Create an uninitialized mutable array.
new :: forall s. Int -> ST s (MArray s)
new n
  | n < 0 || n .&. highBit /= 0 = array_size_error
  | otherwise = ST $ \s1# ->
       case newByteArray# len# s1# of
         (# s2#, marr# #) -> (# s2#, MArray marr# #)
  where !(I# len#) = bytesInArray n
        highBit    = maxBound `xor` (maxBound `shiftR` 1)
{-# INLINE new #-}

array_size_error :: a
array_size_error = error "Data.Text.Array.new: size overflow"

-- | Freeze a mutable array. Do not mutate the 'MArray' afterwards!
unsafeFreeze :: MArray s -> ST s Array
unsafeFreeze MArray{..} = ST $ \s1# ->
    case unsafeFreezeByteArray# maBA s1# of
        (# s2#, ba# #) -> (# s2#, Array ba# #)
{-# INLINE unsafeFreeze #-}

-- | Indicate how many bytes would be used for an array of the given
-- size.
bytesInArray :: Int -> Int
bytesInArray n = n `shiftL` 1
{-# INLINE bytesInArray #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Array -> Int -> Word16
unsafeIndex a@Array{..} i@(I# i#) =
#if defined(ASSERTS)
  let word16len = I# (sizeofByteArray# aBA) `quot` 2 in
  if i < 0 || i >= word16len
  then error ("Data.Text.Array.unsafeIndex: bounds error, offset " ++ show i ++ ", length " ++ show word16len)
  else
#endif
  case indexWord16Array# aBA i# of r# -> (W16# r#)
{-# INLINE unsafeIndex #-}

#if defined(ASSERTS)
-- sizeofMutableByteArray# is deprecated, because it is unsafe in the presence of
-- shrinkMutableByteArray# and resizeMutableByteArray#.
getSizeofMArray :: MArray s -> ST s Int
getSizeofMArray ma@MArray{..} = ST $ \s0# ->
  case getSizeofMutableByteArray# maBA s0# of
    (# s1#, word8len# #) -> (# s1#, I# word8len# #)

checkBoundsM :: HasCallStack => MArray s -> Int -> Int -> ST s ()
checkBoundsM ma i elSize = do
  len <- getSizeofMArray ma
  if i < 0 || i + elSize > len
    then error ("bounds error, offset " ++ show i ++ ", length " ++ show len)
    else return ()
#endif

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWrite ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  MArray s -> Int -> Word16 -> ST s ()
unsafeWrite ma@MArray{..} i@(I# i#) (W16# e#) =
#if defined(ASSERTS)
  checkBoundsM ma (i * 2) 2 >>
#endif
  (ST $ \s1# -> case writeWord16Array# maBA i# e# s1# of
    s2# -> (# s2#, () #))
{-# INLINE unsafeWrite #-}

-- | Convert an immutable array to a list.
toList :: Array -> Int -> Int -> [Word16]
toList ary off len = loop 0
    where loop i | i < len   = unsafeIndex ary (off+i) : loop (i+1)
                 | otherwise = []

-- | An empty immutable array.
empty :: Array
empty = runST (new 0 >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result.
run :: (forall s. ST s (MArray s)) -> Array
run k = runST (k >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result paired with whatever else the action returns.
run2 :: (forall s. ST s (MArray s, a)) -> (Array, a)
run2 k = runST (do
                 (marr,b) <- k
                 arr <- unsafeFreeze marr
                 return (arr,b))
{-# INLINE run2 #-}

-- | Copy some elements of a mutable array.
copyM :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> MArray s               -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ Count
      -> ST s ()
copyM dst@(MArray dst#) dstOff@(I# dstOff#) src@(MArray src#) srcOff@(I# srcOff#) count@(I# count#)
    | I# count# <= 0 = return ()
    | otherwise = do
#if defined(ASSERTS)
    srcLen <- getSizeofMArray src
    dstLen <- getSizeofMArray dst
    assert (srcOff + count <= srcLen `quot` 2) .
      assert (dstOff + count <= dstLen `quot` 2) .
#endif
      ST $ \s1# -> case copyMutableByteArray# src# (2# *# srcOff#) dst# (2# *# dstOff#) (2# *# count#) s1# of
        s2# -> (# s2#, () #)
{-# INLINE copyM #-}

-- | Copy some elements of an immutable array.
copyI :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> Array                  -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ First offset in destination /not/ to
                                -- copy (i.e. /not/ length)
      -> ST s ()
copyI (MArray dst#) dstOff@(I# dstOff#) (Array src#) (I# srcOff#) top@(I# top#)
    | dstOff >= top = return ()
    | otherwise = ST $ \s1# ->
      case copyByteArray# src# (2# *# srcOff#) dst# (2# *# dstOff#) (2# *# (top# -# dstOff#)) s1# of
        s2# -> (# s2#, () #)
{-# INLINE copyI #-}

-- | Compare portions of two arrays for equality.  No bounds checking
-- is performed.
equal :: Array                  -- ^ First
      -> Int                    -- ^ Offset into first
      -> Array                  -- ^ Second
      -> Int                    -- ^ Offset into second
      -> Int                    -- ^ Count
      -> Bool
equal (Array src1#) (I# off1#) (Array src2#) (I# off2#) (I# count#) = i == 0
  where
#if MIN_VERSION_base(4,11,0)
    i = I# (compareByteArrays# src1# (2# *# off1#) src2# (2# *# off2#) (2# *# count#))
#else
    i = inlinePerformIO (memcmp src1# off1# src2# off2# count#)

foreign import ccall unsafe "_hs_text_memcmp" memcmp
    :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> IO Int
#endif
{-# INLINE equal #-}
