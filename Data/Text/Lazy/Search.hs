{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

-- |
-- Module      : Data.Text.Lazy.Search
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtharper@aftereternity.co.uk,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- Fast substring search for 'Text', based on work by Boyer, Moore,
-- Horspool, Sunday, and Lundh.
--
-- References:
-- 
-- * R. S. Boyer, J. S. Moore: A Fast String Searching Algorithm.
--   Communications of the ACM, 20, 10, 762-772 (1977)
--
-- * R. N. Horspool: Practical Fast Searching in Strings.  Software -
--   Practice and Experience 10, 501-506 (1980)
--
-- * D. M. Sunday: A Very Fast Substring Search Algorithm.
--   Communications of the ACM, 33, 8, 132-142 (1990)
--
-- * F. Lundh: The Fast Search Algorithm.
--   <http://effbot.org/zone/stringlib.htm> (2006)

module Data.Text.Lazy.Search
    (
      indices
    ) where

import qualified Data.Text.Array as A
import Data.Int (Int64)
import Data.Word (Word64)
import qualified Data.Text.Internal as T
import Data.Text.Fusion.Internal (PairS(..))
import Data.Text.Lazy.Internal (Text(..))
import Data.Bits ((.|.), (.&.))
import Data.Text.UnsafeShift (shiftL)

-- | /O(n+m)/ Find the offsets of all non-overlapping indices of
-- @needle@ within @haystack@.
--
-- In (unlikely) bad cases, this algorithm's complexity degrades
-- towards /O(n*m)/.
indices :: Text              -- ^ Substring to search for (@needle@)
        -> Text              -- ^ Text to search in (@haystack@)
        -> [Int64]
indices = undefined

foldChunks :: (a -> T.Text -> a) -> a -> Text -> a
foldChunks _ z Empty        = z
foldChunks f z (Chunk c cs) = let z' = f z c
                              in z' `seq` foldChunks f z' cs

wordLength = foldChunks sumLength 0
    where sumLength i (T.Text _ _ l) = i + fromIntegral l

buildTable :: Text -> PairS Word64 Int64
buildTable Empty = 0 :*: 0
buildTable needle@(Chunk c cs) = outer c cs 0 0 (nlen-2)
  where
    outer x@(T.Text xarr xoff xlen) xs = go
      where
        go !i !mask !skip
            | i >= xlen = case xs of
                            Empty      -> (mask .|. swizzle z) :*: skip
                            Chunk y ys -> outer y ys 0 mask skip
            | otherwise = go (i+1) (mask .|. swizzle c) skip'
            where c                 = A.unsafeIndex xarr (xoff+i)
                  skip' | c == z    = nlen - fromIntegral i - 2
                        | otherwise = skip
    swizzle k = 1 `shiftL` (fromIntegral k .&. 0x3f)
    nlen      = wordLength needle
    z         = foldChunks fin 0 needle
        where fin _ (T.Text narr noff nlen) = A.unsafeIndex narr (noff+nlen-1)
