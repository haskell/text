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
import Data.Word (Word16, Word64)
import qualified Data.Text.Internal as T
import qualified Data.Text as T
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
indices needle@(Chunk n ns) haystack@(Chunk k ks)
    | nlen <= 0 || ldiff < 0 = []
    | nlen == 1              = scanOne (nindex 0) 0 k ks
    | otherwise              = scan 0 0 k ks
  where
    scan !g !i x@(T.Text _ _ l) xs
         | g > ldiff                  = []
         | i >= m                     = case xs of
                                          Empty      -> []
                                          Chunk y ys -> scan g (i-m) y ys
         | c == z && candidateMatch 0 = g : scan (g+nlen) (i+nlen) x xs
         | otherwise                  = scan (g+delta) (i+delta) x xs
       where
         m = fromIntegral l
         c = hindex (i + nlast)
         delta | nextInPattern = nlen + 1
               | c == z        = skip + 1
               | otherwise     = 1
         nextInPattern         = mask .&. swizzle (hindex (i+nlen)) == 0
         candidateMatch !j
             | j >= nlast               = True
             | hindex (i+j) /= nindex j = False
             | otherwise                = candidateMatch (j+1)
         hindex                = index x xs
    ldiff     = hlen - nlen
    hlen      = wordLength haystack
    nlen      = wordLength needle
    nlast     = nlen - 1
    nindex    = index n ns
    z         = foldChunks fin 0 needle
        where fin _ (T.Text farr foff flen) = A.unsafeIndex farr (foff+flen-1)
    mask :*: skip = buildTable needle
    scanOne c i (T.Text oarr ooff olen) os = go 0
      where
        go h | h >= olen = case os of
                             Empty      -> []
                             Chunk y ys -> scanOne c (i+fromIntegral olen) y ys
             | on == c = i + fromIntegral h : go (h+1)
             | otherwise = go (h+1)
             where on = A.unsafeIndex oarr (ooff+h)
indices _ _ = []

index :: T.Text -> Text -> Int64 -> Word16
index (T.Text arr off len) xs i
    | j < len   = A.unsafeIndex arr (off+j)
    | otherwise = case xs of
                    Empty | j == len  -> 0
                          | otherwise -> error "empty"
                    Chunk c cs -> index c cs (i-fromIntegral len)
    where j = fromIntegral i

swizzle :: Word16 -> Word64
swizzle k = 1 `shiftL` (fromIntegral k .&. 0x3f)

foldChunks :: (a -> T.Text -> a) -> a -> Text -> a
foldChunks _ z Empty        = z
foldChunks f z (Chunk c cs) = let z' = f z c
                              in z' `seq` foldChunks f z' cs

wordLength :: Text -> Int64
wordLength = foldChunks sumLength 0
    where sumLength i (T.Text _ _ l) = i + fromIntegral l

buildTable :: Text -> PairS Word64 Int64
buildTable Empty = 0 :*: 0
buildTable needle@(Chunk k ks) = outer k ks 0 0 0 (nlen-2)
  where
    outer (T.Text xarr xoff xlen) xs = go
      where
        go !(g::Int64) !i !mask !skip
            | i >= xlast = case xs of
                             Empty      -> (mask .|. swizzle z) :*: skip
                             Chunk y ys -> outer y ys g 0 mask skip
            | otherwise = go (g+1) (i+1) (mask .|. swizzle c) skip'
            where c                 = A.unsafeIndex xarr (xoff+i)
                  skip' | c == z    = nlen - fromIntegral g - 2
                        | otherwise = skip
                  xlast = xlen - 1
    nlen      = wordLength needle
    z         = foldChunks fin 0 needle
        where fin _ (T.Text farr foff flen) = A.unsafeIndex farr (foff+flen-1)
