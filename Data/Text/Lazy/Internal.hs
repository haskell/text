{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}
-- |
-- Module      : Data.Text.Lazy.Internal
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
-- 
-- A module containing semi-public 'Text' internals. This exposes the
-- 'Text' representation and low level construction functions.
-- Modules which extend the 'Text' system may need to use this module.
-- Regular users should not.
module Data.Text.Lazy.Internal
    (
      Text(..)
    , chunk
    , empty
    , foldrChunks
    , foldlChunks
    -- * Data type invariant and abstraction functions
    , invariant
    , checkInvariant
    , showStructure
    -- * Chunk allocation sizes
    , defaultChunkSize
    , smallChunkSize
    , chunkOverhead
    ) where

import qualified Data.Text.Internal as T
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Foreign.Storable (sizeOf)

data Text = Empty
          | Chunk {-# UNPACK #-} !T.Text Text
            deriving (Typeable)

-- | The data type invariant: Every 'Text' is either 'Empty' or
-- consists of non-null 'T.Text's.  All functions must preserve this,
-- and the QC properties must check this.
invariant :: Text -> Bool
invariant Empty                       = True
invariant (Chunk (T.Text _ _ len) cs) = len > 0 && invariant cs

showStructure :: Text -> String
showStructure Empty           = "Empty"
showStructure (Chunk t Empty) = "Chunk " ++ show t ++ " Empty"
showStructure (Chunk t ts)    =
    "Chunk " ++ show t ++ " (" ++ showStructure ts ++ ")"

-- | In a form that checks the invariant lazily.
checkInvariant :: Text -> Text
checkInvariant Empty = Empty
checkInvariant (Chunk c@(T.Text _ _ len) cs)
    | len > 0   = Chunk c (checkInvariant cs)
    | otherwise = error $ "Data.Text.Lazy: invariant violation: "
               ++ showStructure (Chunk c cs)

-- | Smart constructor for 'Chunk'. Guarantees the data type invariant.
chunk :: T.Text -> Text -> Text
{-# INLINE chunk #-}
chunk t@(T.Text _ _ len) ts | len == 0 = ts
                            | otherwise = Chunk t ts

-- | Smart constructor for 'Empty'.
empty :: Text
{-# INLINE [0] empty #-}
empty = Empty

-- | Consume the chunks of a lazy 'Text' with a natural right fold.
foldrChunks :: (T.Text -> a -> a) -> a -> Text -> a
foldrChunks f z = go
  where go Empty        = z
        go (Chunk c cs) = f c (go cs)
{-# INLINE foldrChunks #-}

-- | Consume the chunks of a lazy 'Text' with a strict, tail-recursive,
-- accumulating left fold.
foldlChunks :: (a -> T.Text -> a) -> a -> Text -> a
foldlChunks f z = go z
  where go !a Empty        = a
        go !a (Chunk c cs) = go (f a c) cs
{-# INLINE foldlChunks #-}

-- | Currently set to 32k, less the memory management overhead.
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
   where k = 1024 `div` sizeOf (undefined :: Word16)

-- | Currently set to 4k, less the memory management overhead.
smallChunkSize :: Int
smallChunkSize = 4 * k - chunkOverhead
   where k = 1024 `div` sizeOf (undefined :: Word16)

-- | The memory management overhead. Currently this is tuned for GHC only.
chunkOverhead :: Int
chunkOverhead = 2 * sizeOf (undefined :: Int)
