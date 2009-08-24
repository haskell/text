{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Data.Text.Search where

import qualified Data.Text as T ()
import qualified Data.Text.Array as A
import Data.Word (Word64)
import Data.Text.Internal (Text(..))
import Data.Bits ((.|.), (.&.))
import Data.Text.UnsafeShift (shiftL)

indices :: Text -> Text -> [Int]
indices _needle@(Text narr noff nlen) _haystack@(Text harr hoff hlen)
  | ldiff < 0 = []
  | otherwise = outer 0
  where
    ldiff   = hlen - nlen
    nlast   = nlen - 1
    z       = nindex nlast
    nindex k = A.unsafeIndex narr (noff+k)
    hindex k = A.unsafeIndex harr (hoff+k)
    (mask :: Word64, skip :: Int) = buildTable 0 0 (nlen-2)
    buildTable !i !msk !skp
        | i >= nlast           = (msk .|. swizzle z, skp)
        | otherwise            = buildTable (i+1) (msk .|. swizzle c) skp'
        where c                = nindex i
              skp' | c == z    = nlen - i - 2
                   | otherwise = skp
    swizzle k = 1 `shiftL` (fromEnum k .&. 0x3f)
    outer !i
        | i > ldiff = []
        | c == z    = if candidateMatch 0
                      then i : outer (i + nlast)
                      else if nextInPattern
                           then outer (i + nlen)
                           else outer (i + skip)
        | nextInPattern = outer (i + nlen)
        | otherwise     = outer (i + 1)
        where c = hindex (i+nlast)
              candidateMatch j
                  | j >= nlast               = True
                  | hindex (i+j) /= nindex j = False
                  | otherwise                = candidateMatch (j+1)
              nextInPattern = (mask .&. swizzle (hindex (i+nlen))) == 0
