{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, RecordWildCards,
    UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- |
-- Module      : Data.Text.Array
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
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
      Array(aBA)
    , MArray(maBA)

    -- * Functions
    , copyM
    , copyI
    , empty
    , equal
#if defined(ASSERTS)
    , length
#endif
    , run
    , run2
    , toList
    , unsafeFreeze
    , unsafeIndex
    , new
    , unsafeWrite
    ) where

#if defined(ASSERTS)
-- This fugly hack is brought by GHC's apparent reluctance to deal
-- with MagicHash and UnboxedTuples when inferring types. Eek!
# define CHECK_BOUNDS(_func_,_len_,_k_) \
if (_k_) < 0 || (_k_) >= (_len_) then error ("Data.Text.Array." ++ (_func_) ++ ": bounds error, offset " ++ show (_k_) ++ ", length " ++ show (_len_)) else
#else
# define CHECK_BOUNDS(_func_,_len_,_k_)
#endif

#include "MachDeps.h"

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.Bits ((.&.), xor)
import Data.Text.UnsafeShift (shiftL, shiftR)
import GHC.Base (ByteArray#, MutableByteArray#, Int(..),
                 indexWord16Array#, indexWordArray#, newByteArray#,
                 readWord16Array#, readWordArray#, unsafeCoerce#,
                 writeWord16Array#, writeWordArray#)
import GHC.ST (ST(..), runST)
import GHC.Word (Word16(..), Word(..))
import Prelude hiding (length, read)

-- | Immutable array type.
data Array = Array {
      aBA :: ByteArray#
#if defined(ASSERTS)
    , aLen :: {-# UNPACK #-} !Int -- length (in units of Word16, not bytes)
#endif
    }

-- | Mutable array type, for use in the ST monad.
data MArray s = MArray {
      maBA :: MutableByteArray# s
#if defined(ASSERTS)
    , maLen :: {-# UNPACK #-} !Int -- length (in units of Word16, not bytes)
#endif
    }

#if defined(ASSERTS)
-- | Operations supported by all arrays.
class IArray a where
    -- | Return the length of an array.
    length :: a -> Int

instance IArray Array where
    length = aLen
    {-# INLINE length #-}

instance IArray (MArray s) where
    length = maLen
    {-# INLINE length #-}
#endif

-- | Create an uninitialized mutable array.
new :: forall s. Int -> ST s (MArray s)
new n
  | n < 0 || n .&. highBit /= 0 = error $ "Data.Text.Array.new: size overflow"
  | otherwise = ST $ \s1# ->
       case newByteArray# len# s1# of
         (# s2#, marr# #) -> (# s2#, MArray marr#
#if defined(ASSERTS)
                                n
#endif
                                #)
  where !(I# len#) = bytesInArray n
        highBit    = maxBound `xor` (maxBound `shiftR` 1)
{-# INLINE new #-}

-- | Freeze a mutable array. Do not mutate the 'MArray' afterwards!
unsafeFreeze :: MArray s -> ST s Array
unsafeFreeze MArray{..} = ST $ \s# ->
                          (# s#, Array (unsafeCoerce# maBA)
#if defined(ASSERTS)
                             maLen
#endif
                             #)
{-# INLINE unsafeFreeze #-}

-- | Indicate how many bytes would be used for an array of the given
-- size.
bytesInArray :: Int -> Int
bytesInArray n = n `shiftL` 1
{-# INLINE bytesInArray #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex :: Array -> Int -> Word16
unsafeIndex Array{..} i@(I# i#) =
  CHECK_BOUNDS("unsafeIndex",aLen,i)
    case indexWord16Array# aBA i# of r# -> (W16# r#)
{-# INLINE unsafeIndex #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndexWord :: Array -> Int -> Word
unsafeIndexWord Array{..} i@(I# i#) =
  CHECK_BOUNDS("unsafeIndexWord",aLen`div`wordFactor,i)
    case indexWordArray# aBA i# of r# -> (W# r#)
{-# INLINE unsafeIndexWord #-}

-- | Unchecked read of a mutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeRead :: MArray s -> Int -> ST s Word16
unsafeRead MArray{..} i@(I# i#) = ST $ \s# ->
  CHECK_BOUNDS("unsafeRead",maLen,i)
  case readWord16Array# maBA i# s# of
    (# s2#, r# #) -> (# s2#, W16# r# #)
{-# INLINE unsafeRead #-}

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWrite :: MArray s -> Int -> Word16 -> ST s ()
unsafeWrite MArray{..} i@(I# i#) (W16# e#) = ST $ \s1# ->
  CHECK_BOUNDS("unsafeWrite",maLen,i)
  case writeWord16Array# maBA i# e# s1# of
    s2# -> (# s2#, () #)
{-# INLINE unsafeWrite #-}

-- | Unchecked read of a mutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeReadWord :: MArray s -> Int -> ST s Word
unsafeReadWord MArray{..} i@(I# i#) = ST $ \s# ->
  CHECK_BOUNDS("unsafeRead64",maLen`div`wordFactor,i)
  case readWordArray# maBA i# s# of
    (# s2#, r# #) -> (# s2#, W# r# #)
{-# INLINE unsafeReadWord #-}

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWriteWord :: MArray s -> Int -> Word -> ST s ()
unsafeWriteWord MArray{..} i@(I# i#) (W# e#) = ST $ \s1# ->
  CHECK_BOUNDS("unsafeWriteWord",maLen`div`wordFactor,i)
  case writeWordArray# maBA i# e# s1# of
    s2# -> (# s2#, () #)
{-# INLINE unsafeWriteWord #-}

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

-- | The amount to divide or multiply by to switch between units of
-- 'Word16' and units of 'Word'.
wordFactor :: Int
wordFactor = SIZEOF_HSWORD `shiftR` 1

-- | Indicate whether an offset is word-aligned.
wordAligned :: Int -> Bool
wordAligned i = i .&. (wordFactor - 1) == 0

-- | Copy some elements of a mutable array.
copyM :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> MArray s               -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ Count
      -> ST s ()
copyM dest didx src sidx count =
#if defined(ASSERTS)
    assert (sidx + count <= length src) .
    assert (didx + count <= length dest) $
#endif
    if srem == 0 && drem == 0
    then fast_loop 0
    else slow_loop 0
    where
      (swidx,srem) = sidx `divMod` wordFactor
      (dwidx,drem) = didx `divMod` wordFactor
      nwds         = count `div` wordFactor
      fast_loop !i
          | i >= nwds = slow_loop (i * wordFactor)
          | otherwise = do w <- unsafeReadWord src (swidx+i)
                           unsafeWriteWord dest (dwidx+i) w
                           fast_loop (i+1)
      slow_loop !i
          | i >= count= return ()
          | otherwise = do unsafeRead src (sidx+i) >>= unsafeWrite dest (didx+i)
                           slow_loop (i+1)

-- | Copy some elements of an immutable array.
copyI :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> Array                  -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ First offset in source /not/ to
                                -- copy (i.e. /not/ length)
      -> ST s ()
copyI dest i0 src j0 top
    | wordAligned i0 && wordAligned j0 = fast (i0 `div` wordFactor) (j0 `div` wordFactor)
    | otherwise = slow i0 j0
  where
    topwds = top `div` wordFactor
    fast !i !j
        | i >= topwds = slow (i * wordFactor) (j * wordFactor)
        | otherwise   = do unsafeWriteWord dest i (src `unsafeIndexWord` j)
                           fast (i+1) (j+1)
    slow !i !j
        | i >= top  = return ()
        | otherwise = do unsafeWrite dest i (src `unsafeIndex` j)
                         slow (i+1) (j+1)

-- | Compare portions of two arrays for equality.  No bounds checking
-- is performed.
equal :: Array                  -- ^ First
      -> Int                    -- ^ Offset into first
      -> Array                  -- ^ Second
      -> Int                    -- ^ Offset into second
      -> Int                    -- ^ Count
      -> Bool
equal arrA offA arrB offB count
    | wordAligned offA && wordAligned offB = fast 0
    | otherwise                            = slow 0
  where
    countWords = count `div` wordFactor
    fast !i
        | i >= countWords = slow (i * wordFactor)
        | a /= b          = False
        | otherwise       = fast (i+1)
        where a     = unsafeIndexWord arrA (offAW+i)
              b     = unsafeIndexWord arrB (offBW+i)
              offAW = offA `div` wordFactor
              offBW = offB `div` wordFactor
    slow !i
        | i >= count = True
        | a /= b     = False
        | otherwise  = slow (i+1)
        where a = unsafeIndex arrA (offA+i)
              b = unsafeIndex arrB (offB+i)
