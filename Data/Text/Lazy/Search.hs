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
-- Fast substring search for lazy 'Text', based on work by Boyer,
-- Moore, Horspool, Sunday, and Lundh.  Adapted from the strict
-- implementation.

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
import Data.Text.Lazy.Internal (Text(..), foldlChunks)
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
    ldiff     = wordLength haystack - nlen
    nlen      = wordLength needle
    nlast     = nlen - 1
    nindex    = index n ns
    z         = foldlChunks fin 0 needle
        where fin _ (T.Text farr foff flen) = A.unsafeIndex farr (foff+flen-1)
    (mask :: Word64) :*: skip = buildTable n ns 0 0 0 (nlen-2)
    swizzle w = 1 `shiftL` (fromIntegral w .&. 0x3f)
    buildTable (T.Text xarr xoff xlen) xs = go
      where
        go !(g::Int64) !i !msk !skp
            | i >= xlast = case xs of
                             Empty      -> (msk .|. swizzle z) :*: skp
                             Chunk y ys -> buildTable y ys g 0 msk skp
            | otherwise = go (g+1) (i+1) (msk .|. swizzle c) skp'
            where c                = A.unsafeIndex xarr (xoff+i)
                  skp' | c == z    = nlen - fromIntegral g - 2
                       | otherwise = skp
                  xlast = xlen - 1
    scanOne c i (T.Text oarr ooff olen) os = go 0
      where
        go h | h >= olen = case os of
                             Empty      -> []
                             Chunk y ys -> scanOne c (i+fromIntegral olen) y ys
             | on == c = i + fromIntegral h : go (h+1)
             | otherwise = go (h+1)
             where on = A.unsafeIndex oarr (ooff+h)
indices _ _ = []

-- | Fast index into a partly unpacked 'Text'.  We take into account
-- the possibility that the caller might try to access one element
-- past the end.
index :: T.Text -> Text -> Int64 -> Word16
index (T.Text arr off len) xs i
    | j < len   = A.unsafeIndex arr (off+j)
    | otherwise = case xs of
                    Empty | j == len  -> 0 -- out of bounds, but legal
                          | otherwise -> emptyError "index"
                    Chunk c cs -> index c cs (i-fromIntegral len)
    where j = fromIntegral i

-- | The number of 'Word16' values in a 'Text'.
wordLength :: Text -> Int64
wordLength = foldlChunks sumLength 0
    where sumLength i (T.Text _ _ l) = i + fromIntegral l

emptyError :: String -> a
emptyError fun = error ("Data.Text.Lazy.Search." ++ fun ++ ": empty input")
