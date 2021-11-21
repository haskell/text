{-# LANGUAGE BangPatterns, CPP, MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Fusion
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009-2010,
--               (c) Duncan Coutts 2009
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
-- Text manipulation functions represented as fusible operations over
-- streams.
module Data.Text.Internal.Fusion
    (
    -- * Types
      Stream(..)
    , Step(..)

    -- * Creation and elimination
    , stream
    , unstream
    , reverseStream

    , length

    -- * Transformations
    , reverse

    -- * Construction
    -- ** Scans
    , reverseScanr

    -- ** Accumulating maps
    , mapAccumL

    -- ** Generation and unfolding
    , unfoldrN

    -- * Indexing
    , index
    , findIndex
    , countChar
    ) where

import Prelude (Bool(..), Char, Maybe(..), Monad(..), Int,
                Num(..), Ord(..), ($),
                otherwise)
import Data.Bits (shiftL, shiftR)
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Private (runText)
import Data.Text.Internal.Unsafe.Char (unsafeChr8, unsafeWrite)
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Fusion.Common as S
import Data.Text.Internal.Fusion.Types
import Data.Text.Internal.Fusion.Size
import qualified Data.Text.Internal as I
import qualified Data.Text.Internal.Encoding.Utf8 as U8
import GHC.Stack (HasCallStack)

default(Int)

-- | /O(n)/ Convert 'Text' into a 'Stream' 'Char'.
--
-- __Properties__
--
-- @'unstream' . 'stream' = 'Data.Function.id'@
--
-- @'stream' . 'unstream' = 'Data.Function.id' @
stream ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> Stream Char
stream (Text arr off len) = Stream next off (betweenSize (len `shiftR` 2) len)
    where
      !end = off+len
      next !i
          | i >= end  = Done
          | otherwise = Yield chr (i + l)
          where
            n0 = A.unsafeIndex arr i
            n1 = A.unsafeIndex arr (i + 1)
            n2 = A.unsafeIndex arr (i + 2)
            n3 = A.unsafeIndex arr (i + 3)

            l  = U8.utf8LengthByLeader n0
            chr = case l of
              1 -> unsafeChr8 n0
              2 -> U8.chr2 n0 n1
              3 -> U8.chr3 n0 n1 n2
              _ -> U8.chr4 n0 n1 n2 n3
{-# INLINE [0] stream #-}

-- | /O(n)/ Converts 'Text' into a 'Stream' 'Char', but iterates
-- backwards through the text.
--
-- __Properties__
--
-- @'unstream' . 'reverseStream' = 'Data.Text.reverse' @
reverseStream :: Text -> Stream Char
reverseStream (Text arr off len) = Stream next (off+len-1) (betweenSize (len `shiftR` 2) len)
    where
      {-# INLINE next #-}
      next !i
          | i < off    = Done
          | n0 <  0x80 = Yield (unsafeChr8 n0)       (i - 1)
          | n1 >= 0xC0 = Yield (U8.chr2 n1 n0)       (i - 2)
          | n2 >= 0xC0 = Yield (U8.chr3 n2 n1 n0)    (i - 3)
          | otherwise  = Yield (U8.chr4 n3 n2 n1 n0) (i - 4)
          where
            n0 = A.unsafeIndex arr i
            n1 = A.unsafeIndex arr (i - 1)
            n2 = A.unsafeIndex arr (i - 2)
            n3 = A.unsafeIndex arr (i - 3)
{-# INLINE [0] reverseStream #-}

-- | /O(n)/ Convert 'Stream' 'Char' into a 'Text'.
--
-- __Properties__
--
-- @'unstream' . 'stream' = 'Data.Function.id'@
--
-- @'stream' . 'unstream' = 'Data.Function.id' @
unstream :: Stream Char -> Text
unstream (Stream next0 s0 len) = runText $ \done -> do
  -- Before encoding each char we perform a buffer realloc check assuming
  -- worst case encoding size of four 8-bit units for the char. Just add an
  -- extra space to the buffer so that we do not end up reallocating even when
  -- all the chars are encoded as single unit.
  let mlen = upperBound 4 len + 3
  arr0 <- A.new mlen
  let outer !arr !maxi = encode
       where
        -- keep the common case loop as small as possible
        encode !si !di =
            case next0 si of
                Done        -> done arr di
                Skip si'    -> encode si' di
                Yield c si'
                    -- simply check for the worst case
                    | maxi < di + 3 -> realloc si di
                    | otherwise -> do
                            n <- unsafeWrite arr di c
                            encode si' (di + n)

        -- keep uncommon case separate from the common case code
        {-# NOINLINE realloc #-}
        realloc !si !di = do
            let newlen = (maxi + 1) * 2
            arr' <- A.resizeM arr newlen
            outer arr' (newlen - 1) si di

  outer arr0 (mlen - 1) s0 0
{-# INLINE [0] unstream #-}
{-# RULES "STREAM stream/unstream fusion" forall s. stream (unstream s) = s #-}


-- ----------------------------------------------------------------------------
-- * Basic stream functions

-- | /O(n)/ Returns the number of characters in a 'Stream'.
--
-- __Properties__
--
-- @'length' . 'stream' = 'Data.Text.length' @
length :: Stream Char -> Int
length = S.lengthI
{-# INLINE[0] length #-}

-- | /O(n)/ Reverse the characters of a 'Stream' returning 'Text'.
--
-- __Properties__
--
-- @'reverse' . 'stream' = 'Data.Text.reverse' @
reverse ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Stream Char -> Text
reverse (Stream next s len0)
    | isEmpty len0 = I.empty
    | otherwise    = I.text arr off' len'
  where
    len0' = upperBound 4 (larger len0 4)
    (arr, (off', len')) = A.run2 (A.new len0' >>= loop s (len0'-1) len0')
    loop !s0 !i !len marr =
        case next s0 of
          Done -> return (marr, (j, len-j))
              where j = i + 1
          Skip s1    -> loop s1 i len marr
          Yield x s1 | i < least -> {-# SCC "reverse/resize" #-} do
                       let newLen = len `shiftL` 1
                       marr' <- A.new newLen
                       A.copyM marr' (newLen-len) marr 0 len
                       _ <- unsafeWrite marr' (len + i - least) x
                       loop s1 (len + i - least - 1) newLen marr'
                     | otherwise -> do
                       _ <- unsafeWrite marr (i - least) x
                       loop s1 (i - least - 1) len marr
            where least = U8.utf8Length x - 1
{-# INLINE [0] reverse #-}

-- | /O(n)/ Perform the equivalent of 'scanr' over a list, only with
-- the input and result reversed.
--
-- __Properties__
--
-- @'reverse' . 'reverseScanr' f c . 'reverseStream' = 'Data.Text.scanr' f c @
reverseScanr :: (Char -> Char -> Char) -> Char -> Stream Char -> Stream Char
reverseScanr f z0 (Stream next0 s0 len) = Stream next (Scan1 z0 s0) (len+1) -- HINT maybe too low
  where
    {-# INLINE next #-}
    next (Scan1 z s) = Yield z (Scan2 z s)
    next (Scan2 z s) = case next0 s of
                         Yield x s' -> let !x' = f x z
                                       in Yield x' (Scan2 x' s')
                         Skip s'    -> Skip (Scan2 z s')
                         Done       -> Done
{-# INLINE reverseScanr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a stream from a seed
-- value. However, the length of the result is limited by the
-- first argument to 'unfoldrN'. This function is more efficient than
-- 'unfoldr' when the length of the result is known.
--
-- __Properties__
--
-- @'unstream' ('unfoldrN' n f a) = 'Data.Text.unfoldrN' n f a @
unfoldrN :: Int -> (a -> Maybe (Char,a)) -> a -> Stream Char
unfoldrN n = S.unfoldrNI n
{-# INLINE [0] unfoldrN #-}

-------------------------------------------------------------------------------
-- ** Indexing streams

-- | /O(n)/ stream index (subscript) operator, starting from 0.
--
-- __Properties__
--
-- @'index' ('stream' t) n  = 'Data.Text.index' t n @
index :: HasCallStack => Stream Char -> Int -> Char
index = S.indexI
{-# INLINE [0] index #-}

-- | The 'findIndex' function takes a predicate and a stream and
-- returns the index of the first element in the stream
-- satisfying the predicate.
--
-- __Properties__
--
-- @'findIndex' p . 'stream'  = 'Data.Text.findIndex' p @
findIndex :: (Char -> Bool) -> Stream Char -> Maybe Int
findIndex = S.findIndexI
{-# INLINE [0] findIndex #-}

-- | /O(n)/ The 'count' function returns the number of times the query
-- element appears in the given stream.
--
-- __Properties__
--
-- @'countChar' c . 'stream'  = 'Data.Text.countChar' c @
countChar :: Char -> Stream Char -> Int
countChar = S.countCharI
{-# INLINE [0] countChar #-}

-- | /O(n)/ Like a combination of 'map' and 'foldl''. Applies a
-- function to each element of a 'Text', passing an accumulating
-- parameter from left to right, and returns a final 'Text'.
--
-- __Properties__
--
-- @'mapAccumL' g z0 . 'stream' = 'Data.Text.mapAccumL' g z0@
mapAccumL ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  (a -> Char -> (a,Char)) -> a -> Stream Char -> (a, Text)
mapAccumL f z0 (Stream next0 s0 len) = (nz, I.text na 0 nl)
  where
    (na,(nz,nl)) = A.run2 (A.new mlen >>= \arr -> outer arr mlen z0 s0 0)
      where mlen = upperBound 4 len
    outer arr top = loop
      where
        loop !z !s !i =
            case next0 s of
              Done          -> return (arr, (z,i))
              Skip s'       -> loop z s' i
              Yield x s'
                | j >= top  -> {-# SCC "mapAccumL/resize" #-} do
                               let top' = (top + 1) `shiftL` 1
                               arr' <- A.resizeM arr top'
                               outer arr' top' z s i
                | otherwise -> do d <- unsafeWrite arr i c
                                  loop z' s' (i+d)
                where (z',c) = f z x
                      j = i + U8.utf8Length c - 1
{-# INLINE [0] mapAccumL #-}
