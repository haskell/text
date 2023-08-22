{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.Text.Internal
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- A module containing private 'Text' internals. This exposes the
-- 'Text' representation and low level construction functions.
-- Modules which extend the 'Text' system may need to use this module.
--
-- You should not use this module unless you are determined to monkey
-- with the internals, as the functions here do just about nothing to
-- preserve data invariants.  You have been warned!

module Data.Text.Internal
    (
    -- * Types
    -- $internals
      Text(..)
    , StrictText
    -- * Construction
    , text
    , textP
    -- * Safety
    , safe
    -- * Code that must be here for accessibility
    , empty
    , append
    -- * Utilities
    , firstf
    -- * Checked multiplication
    , mul
    , mul32
    , mul64
    -- * Debugging
    , showText
    -- * Conversions
    , pack
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
import GHC.Stack (HasCallStack)
#endif
import Control.Monad.ST (ST, runST)
import Data.Bits
import Data.Int (Int32, Int64)
import Data.Text.Internal.Unsafe.Char (ord, unsafeWrite)
import Data.Typeable (Typeable)
import qualified Data.Text.Array as A

-- | A space efficient, packed, unboxed Unicode text type.
data Text = Text
    {-# UNPACK #-} !A.Array -- ^ bytearray encoded as UTF-8
    {-# UNPACK #-} !Int     -- ^ offset in bytes (not in Char!), pointing to a start of UTF-8 sequence
    {-# UNPACK #-} !Int     -- ^ length in bytes (not in Char!), pointing to an end of UTF-8 sequence
    deriving (Typeable)

-- | Type synonym for the strict flavour of 'Text'.
type StrictText = Text

-- | Smart constructor.
text_ ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
     A.Array -- ^ bytearray encoded as UTF-8
  -> Int     -- ^ offset in bytes (not in Char!), pointing to a start of UTF-8 sequence
  -> Int     -- ^ length in bytes (not in Char!), pointing to an end of UTF-8 sequence
  -> Text
text_ arr off len =
#if defined(ASSERTS)
  let c    = A.unsafeIndex arr off
  in assert (len >= 0) .
     assert (off >= 0) .
     assert (len == 0 || c < 0x80 || c >= 0xC0) $
#endif
     Text arr off len
{-# INLINE text_ #-}

-- | /O(1)/ The empty 'Text'.
empty :: Text
empty = Text A.empty 0 0
{-# NOINLINE empty #-}

-- | /O(n)/ Appends one 'Text' to the other by copying both of them
-- into a new 'Text'.
append :: Text -> Text -> Text
append a@(Text arr1 off1 len1) b@(Text arr2 off2 len2)
    | len1 == 0 = b
    | len2 == 0 = a
    | len > 0   = Text (A.run x) 0 len
    | otherwise = error $ "Data.Text.append: size overflow"
    where
      len = len1+len2
      x :: ST s (A.MArray s)
      x = do
        arr <- A.new len
        A.copyI len1 arr 0 arr1 off1
        A.copyI len2 arr len1 arr2 off2
        return arr
{-# NOINLINE append #-}

-- | Construct a 'Text' without invisibly pinning its byte array in
-- memory if its length has dwindled to zero.
-- It ensures that empty 'Text' values are shared.
text ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
     A.Array -- ^ bytearray encoded as UTF-8
  -> Int     -- ^ offset in bytes (not in Char!), pointing to a start of UTF-8 sequence
  -> Int     -- ^ length in bytes (not in Char!), pointing to an end of UTF-8 sequence
  -> Text
text arr off len | len == 0  = empty
                 | otherwise = text_ arr off len
{-# INLINE [0] text #-}

textP :: A.Array -> Int -> Int -> Text
{-# DEPRECATED textP "Use text instead" #-}
textP = text

-- | A useful 'show'-like function for debugging purposes.
showText :: Text -> String
showText (Text arr off len) =
    "Text " ++ show (A.toList arr off len) ++ ' ' :
            show off ++ ' ' : show len

-- | Map a 'Char' to a 'Text'-safe value.
--
-- Unicode 'Data.Char.Surrogate' code points are not included in the set of Unicode
-- scalar values, but are unfortunately admitted as valid 'Char'
-- values by Haskell.  They cannot be represented in a 'Text'.  This
-- function remaps those code points to the Unicode replacement
-- character (U+FFFD, \'&#xfffd;\'), and leaves other code points
-- unchanged.
safe :: Char -> Char
safe c
    | ord c .&. 0x1ff800 /= 0xd800 = c
    | otherwise                    = '\xfffd'
{-# INLINE [0] safe #-}

-- | Apply a function to the first element of an optional pair.
firstf :: (a -> c) -> Maybe (a,b) -> Maybe (c,b)
firstf f (Just (a, b)) = Just (f a, b)
firstf _  Nothing      = Nothing

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul :: Int -> Int -> Int
mul a b
  | finiteBitSize (0 :: Word) == 64
  = int64ToInt $ intToInt64 a `mul64` intToInt64 b
  | otherwise
  = int32ToInt $ intToInt32 a `mul32` intToInt32 b
{-# INLINE mul #-}
infixl 7 `mul`

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul64 :: Int64 -> Int64 -> Int64
mul64 a b
  | a >= 0 && b >= 0 =  mul64_ a b
  | a >= 0           = -mul64_ a (-b)
  | b >= 0           = -mul64_ (-a) b
  | otherwise        =  mul64_ (-a) (-b)
{-# INLINE mul64 #-}
infixl 7 `mul64`

mul64_ :: Int64 -> Int64 -> Int64
mul64_ a b
  | ahi > 0 && bhi > 0 = error "overflow"
  | top > 0x7fffffff   = error "overflow"
  | total < 0          = error "overflow"
  | otherwise          = total
  where (# ahi, alo #) = (# a `shiftR` 32, a .&. 0xffffffff #)
        (# bhi, blo #) = (# b `shiftR` 32, b .&. 0xffffffff #)
        top            = ahi * blo + alo * bhi
        total          = (top `shiftL` 32) + alo * blo
{-# INLINE mul64_ #-}

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul32 :: Int32 -> Int32 -> Int32
mul32 a b = case int32ToInt64 a * int32ToInt64 b of
              ab | ab < min32 || ab > max32 -> error "overflow"
                 | otherwise                -> int64ToInt32 ab
  where min32 = -0x80000000 :: Int64
        max32 =  0x7fffffff
{-# INLINE mul32 #-}
infixl 7 `mul32`

intToInt64 :: Int -> Int64
intToInt64 = fromIntegral

int64ToInt :: Int64 -> Int
int64ToInt = fromIntegral

intToInt32 :: Int -> Int32
intToInt32 = fromIntegral

int32ToInt :: Int32 -> Int
int32ToInt = fromIntegral

int32ToInt64 :: Int32 -> Int64
int32ToInt64 = fromIntegral

int64ToInt32 :: Int64 -> Int32
int64ToInt32 = fromIntegral

-- $internals
--
-- Internally, the 'Text' type is represented as an array of 'Word8'
-- UTF-8 code units. The offset and length fields in the constructor
-- are in these units, /not/ units of 'Char'.
--
-- Invariants that all functions must maintain:
--
-- * Since the 'Text' type uses UTF-8 internally, it cannot represent
--   characters in the reserved surrogate code point range U+D800 to
--   U+DFFF. To maintain this invariant, the 'safe' function maps
--   'Char' values in this range to the replacement character (U+FFFD,
--   \'&#xfffd;\').
--
-- * Offset and length must point to a valid UTF-8 sequence of bytes.
--   Violation of this may cause memory access violation and divergence.

-- -----------------------------------------------------------------------------
-- * Conversion to/from 'Text'

-- | /O(n)/ Convert a 'String' into a 'Text'.
-- Performs replacement on invalid scalar values, so @'Data.Text.unpack' . 'pack'@ is not 'id':
--
-- >>> Data.Text.unpack (pack "\55555")
-- "\65533"
pack :: String -> Text
pack [] = empty
pack xs = runST $ do
  -- It's tempting to allocate a buffer of 4 * length xs bytes,
  -- but not only it's wasteful for predominantly ASCII arguments,
  -- the computation of length xs would force allocation of the entire xs at once.
  let dstLen = 64
  dst <- A.new dstLen
  outer dst dstLen 0 xs
  where
    outer :: forall s. A.MArray s -> Int -> Int -> String -> ST s Text
    outer !dst !dstLen = inner
      where
        inner !dstOff [] = do
          A.shrinkM dst dstOff
          arr <- A.unsafeFreeze dst
          return (Text arr 0 dstOff)
        inner !dstOff ccs@(c : cs)
          -- Each 'Char' takes up to 4 bytes
          | dstOff + 4 > dstLen = do
            -- Double size of the buffer
            let !dstLen' = dstLen * 2
            dst' <- A.resizeM dst dstLen'
            outer dst' dstLen' dstOff ccs
          | otherwise = do
            d <- unsafeWrite dst dstOff (safe c)
            inner (dstOff + d) cs
{-# NOINLINE [0] pack #-}
