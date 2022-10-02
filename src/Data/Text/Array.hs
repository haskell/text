{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

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
      Array
    , pattern ByteArray
    , MArray
    , pattern MutableByteArray
    -- * Functions
    , resizeM
    , shrinkM
    , copyM
    , copyI
    , copyFromPointer
    , copyToPointer
    , empty
    , equal
    , compare
    , run
    , run2
    , toList
    , unsafeFreeze
    , unsafeIndex
    , new
    , newPinned
    , newFilled
    , unsafeWrite
    , tile
    , getSizeofMArray
    ) where

#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif
#if !MIN_VERSION_base(4,11,0)
import Foreign.C.Types (CInt(..))
#endif
import GHC.Exts hiding (toList)
import GHC.ST (ST(..), runST)
import GHC.Word (Word8(..))
import qualified Prelude
import Prelude hiding (length, read, compare)
import Data.Array.Byte (ByteArray(..), MutableByteArray(..))

-- | Immutable array type.
type Array = ByteArray

-- | Mutable array type, for use in the ST monad.
type MArray = MutableByteArray

-- | Create an uninitialized mutable array.
new :: forall s. Int -> ST s (MArray s)
new (I# len#)
#if defined(ASSERTS)
  | I# len# < 0 = error "Data.Text.Array.new: size overflow"
#endif
  | otherwise = ST $ \s1# ->
    case newByteArray# len# s1# of
      (# s2#, marr# #) -> (# s2#, MutableByteArray marr# #)
{-# INLINE new #-}

-- | Create an uninitialized mutable pinned array.
--
-- @since 2.0
newPinned :: forall s. Int -> ST s (MArray s)
newPinned (I# len#)
#if defined(ASSERTS)
  | I# len# < 0 = error "Data.Text.Array.newPinned: size overflow"
#endif
  | otherwise = ST $ \s1# ->
    case newPinnedByteArray# len# s1# of
      (# s2#, marr# #) -> (# s2#, MutableByteArray marr# #)
{-# INLINE newPinned #-}

-- | @since 2.0
newFilled :: Int -> Int -> ST s (MArray s)
newFilled (I# len#) (I# c#) = ST $ \s1# ->
  case newByteArray# len# s1# of
    (# s2#, marr# #) -> case setByteArray# marr# 0# len# c# s2# of
      s3# -> (# s3#, MutableByteArray marr# #)
{-# INLINE newFilled #-}

-- | @since 2.0
tile :: MArray s -> Int -> ST s ()
tile marr tileLen = do
  totalLen <- getSizeofMArray marr
  let go l
        | 2 * l > totalLen = copyM marr l marr 0 (totalLen - l)
        | otherwise = copyM marr l marr 0 l >> go (2 * l)
  go tileLen
{-# INLINE tile #-}

-- | Freeze a mutable array. Do not mutate the 'MArray' afterwards!
unsafeFreeze :: MArray s -> ST s Array
unsafeFreeze (MutableByteArray marr) = ST $ \s1# ->
    case unsafeFreezeByteArray# marr s1# of
        (# s2#, ba# #) -> (# s2#, ByteArray ba# #)
{-# INLINE unsafeFreeze #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Array -> Int -> Word8
unsafeIndex (ByteArray arr) i@(I# i#) =
#if defined(ASSERTS)
  let word8len = I# (sizeofByteArray# arr) in
  if i < 0 || i >= word8len
  then error ("Data.Text.Array.unsafeIndex: bounds error, offset " ++ show i ++ ", length " ++ show word8len)
  else
#endif
  case indexWord8Array# arr i# of r# -> (W8# r#)
{-# INLINE unsafeIndex #-}

-- | @since 2.0
getSizeofMArray :: MArray s -> ST s Int
getSizeofMArray (MutableByteArray marr) = ST $ \s0# ->
  -- Cannot simply use (deprecated) 'sizeofMutableByteArray#', because it is
  -- unsafe in the presence of 'shrinkMutableByteArray#' and 'resizeMutableByteArray#'.
  case getSizeofMutableByteArray# marr s0# of
    (# s1#, word8len# #) -> (# s1#, I# word8len# #)

#if defined(ASSERTS)
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
  MArray s -> Int -> Word8 -> ST s ()
unsafeWrite ma@(MutableByteArray marr) i@(I# i#) (W8# e#) =
#if defined(ASSERTS)
  checkBoundsM ma i 1 >>
#endif
  (ST $ \s1# -> case writeWord8Array# marr i# e# s1# of
    s2# -> (# s2#, () #))
{-# INLINE unsafeWrite #-}

-- | Convert an immutable array to a list.
toList :: Array -> Int -> Int -> [Word8]
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

-- | @since 2.0
resizeM :: MArray s -> Int -> ST s (MArray s)
resizeM (MutableByteArray ma) i@(I# i#) = ST $ \s1# ->
  case resizeMutableByteArray# ma i# s1# of
    (# s2#, newArr #) -> (# s2#, MutableByteArray newArr #)
{-# INLINE resizeM #-}

-- | @since 2.0
shrinkM ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  MArray s -> Int -> ST s ()
shrinkM (MutableByteArray marr) i@(I# newSize) = do
#if defined(ASSERTS)
  oldSize <- getSizeofMArray (MutableByteArray marr)
  if I# newSize > oldSize
    then error $ "shrinkM: shrink cannot grow " ++ show oldSize ++ " to " ++ show (I# newSize)
    else return ()
#endif
  ST $ \s1# ->
    case shrinkMutableByteArray# marr newSize s1# of
      s2# -> (# s2#, () #)
{-# INLINE shrinkM #-}

-- | Copy some elements of a mutable array.
copyM :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> MArray s               -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ Count
      -> ST s ()
copyM dst@(MutableByteArray dst#) dstOff@(I# dstOff#) src@(MutableByteArray src#) srcOff@(I# srcOff#) count@(I# count#)
#if defined(ASSERTS)
  | count < 0 = error $
    "copyM: count must be >= 0, but got " ++ show count
#endif
    | otherwise = do
#if defined(ASSERTS)
    srcLen <- getSizeofMArray src
    dstLen <- getSizeofMArray dst
    if srcOff + count > srcLen
      then error "copyM: source is too short"
      else return ()
    if dstOff + count > dstLen
      then error "copyM: destination is too short"
      else return ()
#endif
    ST $ \s1# -> case copyMutableByteArray# src# srcOff# dst# dstOff# count# s1# of
      s2# -> (# s2#, () #)
{-# INLINE copyM #-}

-- | Copy some elements of an immutable array.
copyI :: Int                    -- ^ Count
      -> MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> Array                  -- ^ Source
      -> Int                    -- ^ Source offset
      -> ST s ()
copyI count@(I# count#) (MutableByteArray dst#) dstOff@(I# dstOff#) (ByteArray src#) (I# srcOff#)
#if defined(ASSERTS)
  | count < 0 = error $
    "copyI: count must be >= 0, but got " ++ show count
#endif
  | otherwise = ST $ \s1# ->
    case copyByteArray# src# srcOff# dst# dstOff# count# s1# of
      s2# -> (# s2#, () #)
{-# INLINE copyI #-}

-- | Copy from pointer.
--
-- @since 2.0
copyFromPointer
  :: MArray s               -- ^ Destination
  -> Int                    -- ^ Destination offset
  -> Ptr Word8              -- ^ Source
  -> Int                    -- ^ Count
  -> ST s ()
copyFromPointer (MutableByteArray dst#) dstOff@(I# dstOff#) (Ptr src#) count@(I# count#)
#if defined(ASSERTS)
  | count < 0 = error $
    "copyFromPointer: count must be >= 0, but got " ++ show count
#endif
  | otherwise = ST $ \s1# ->
    case copyAddrToByteArray# src# dst# dstOff# count# s1# of
      s2# -> (# s2#, () #)
{-# INLINE copyFromPointer #-}

-- | Copy to pointer.
--
-- @since 2.0
copyToPointer
  :: Array                  -- ^ Source
  -> Int                    -- ^ Source offset
  -> Ptr Word8              -- ^ Destination
  -> Int                    -- ^ Count
  -> ST s ()
copyToPointer (ByteArray src#) srcOff@(I# srcOff#) (Ptr dst#) count@(I# count#)
#if defined(ASSERTS)
  | count < 0 = error $
    "copyToPointer: count must be >= 0, but got " ++ show count
#endif
  | otherwise = ST $ \s1# ->
    case copyByteArrayToAddr# src# srcOff# dst# count# s1# of
      s2# -> (# s2#, () #)
{-# INLINE copyToPointer #-}

-- | Compare portions of two arrays for equality.  No bounds checking
-- is performed.
equal :: Array -> Int -> Array -> Int -> Int -> Bool
equal src1 off1 src2 off2 count = compareInternal src1 off1 src2 off2 count == 0
{-# INLINE equal #-}

-- | Compare portions of two arrays. No bounds checking is performed.
--
-- @since 2.0
compare :: Array -> Int -> Array -> Int -> Int -> Ordering
compare src1 off1 src2 off2 count = compareInternal src1 off1 src2 off2 count `Prelude.compare` 0
{-# INLINE compare #-}

compareInternal
      :: Array                  -- ^ First
      -> Int                    -- ^ Offset into first
      -> Array                  -- ^ Second
      -> Int                    -- ^ Offset into second
      -> Int                    -- ^ Count
      -> Int
compareInternal (ByteArray src1#) (I# off1#) (ByteArray src2#) (I# off2#) (I# count#) = i
  where
#if MIN_VERSION_base(4,11,0)
    i = I# (compareByteArrays# src1# off1# src2# off2# count#)
#else
    i = fromIntegral (memcmp src1# off1# src2# off2# count#)

foreign import ccall unsafe "_hs_text_memcmp2" memcmp
    :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> CInt
#endif
{-# INLINE compareInternal #-}
