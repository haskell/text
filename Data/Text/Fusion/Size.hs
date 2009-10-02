{-# OPTIONS_GHC -fno-warn-missing-methods #-}
-- |
-- Module      : Data.Text.Fusion.Internal
-- Copyright   : (c) Roman Leshchinskiy 2008,
--               (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtharper@aftereternity.co.uk,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
--
-- Size hints.

module Data.Text.Fusion.Size
    (
      Size
    , exactSize
    , maxSize
    , unknownSize
    , smaller
    , larger
    , toMax
    , upperBound
    , lowerBound
    , isEmpty
    ) where

import Control.Exception (assert)

data Size = Exact {-# UNPACK #-} !Int -- ^ Exact size.
          | Max   {-# UNPACK #-} !Int -- ^ Upper bound on size.
          | Unknown                   -- ^ Unknown size.
            deriving (Eq, Show)

exactSize :: Int -> Size
exactSize n = assert (n >= 0) Exact n
{-# INLINE exactSize #-}

maxSize :: Int -> Size
maxSize n = assert (n >= 0) Max n
{-# INLINE maxSize #-}

unknownSize :: Size
unknownSize = Unknown
{-# INLINE unknownSize #-}

instance Num Size where
    (+) = addSize
    (-) = subtractSize
    (*) = mulSize

    fromInteger = f where f = Exact . fromInteger
                          {-# INLINE f #-}

addSize :: Size -> Size -> Size
addSize (Exact m) (Exact n) = Exact (m+n)
addSize (Exact m) (Max   n) = Max   (m+n)
addSize (Max   m) (Exact n) = Max   (m+n)
addSize (Max   m) (Max   n) = Max   (m+n)
addSize _          _       = Unknown
{-# INLINE addSize #-}

subtractSize :: Size -> Size -> Size
subtractSize   (Exact m) (Exact n) = Exact (max (m-n) 0)
subtractSize   (Exact m) (Max   _) = Max   m
subtractSize   (Max   m) (Exact n) = Max   (max (m-n) 0)
subtractSize a@(Max   _) (Max   _) = a
subtractSize a@(Max   _) Unknown   = a
subtractSize _         _           = Unknown
{-# INLINE subtractSize #-}

mulSize :: Size -> Size -> Size
mulSize (Exact m) (Exact n) = Exact (m*n)
mulSize (Exact m) (Max   n) = Max   (m*n)
mulSize (Max   m) (Exact n) = Max   (m*n)
mulSize (Max   m) (Max   n) = Max   (m*n)
mulSize _          _       = Unknown
{-# INLINE mulSize #-}

-- | Minimum of two size hints.
smaller :: Size -> Size -> Size
smaller   (Exact m) (Exact n) = Exact (m `min` n)
smaller   (Exact m) (Max   n) = Max   (m `min` n)
smaller   (Exact m) Unknown   = Max   m
smaller   (Max   m) (Exact n) = Max   (m `min` n)
smaller   (Max   m) (Max   n) = Max   (m `min` n)
smaller a@(Max   _) Unknown   = a
smaller   Unknown   (Exact n) = Max   n
smaller   Unknown   (Max   n) = Max   n
smaller   Unknown   Unknown   = Unknown
{-# INLINE smaller #-}

-- | Maximum of two size hints.
larger :: Size -> Size -> Size
larger   (Exact m)   (Exact n)             = Exact (m `max` n)
larger a@(Exact m) b@(Max   n) | m >= n    = a
                               | otherwise = b
larger a@(Max   m) b@(Exact n) | n >= m    = b
                               | otherwise = a
larger   (Max   m)   (Max   n)             = Max   (m `max` n)
larger _             _                     = Unknown
{-# INLINE larger #-}

-- | Convert a size hint to an upper bound.
toMax :: Size -> Size
toMax   (Exact n) = Max n
toMax a@(Max   _) = a
toMax   Unknown   = Unknown
{-# INLINE toMax #-}

-- | Compute the minimum size from a size hint.
lowerBound :: Size -> Int
lowerBound (Exact n) = n
lowerBound _         = 0
{-# INLINE lowerBound #-}

-- | Compute the maximum size from a size hint, if possible.
upperBound :: Int -> Size -> Int
upperBound _ (Exact n) = n
upperBound _ (Max   n) = n
upperBound k _         = k
{-# INLINE upperBound #-}

isEmpty :: Size -> Bool
isEmpty (Exact n) = n <= 0
isEmpty (Max   n) = n <= 0
isEmpty _         = False
{-# INLINE isEmpty #-}
