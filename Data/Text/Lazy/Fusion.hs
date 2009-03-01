-- |
-- Module      : Data.Text.Lazy.Fusion
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : rtharper@aftereternity.co.uk, bos@serpentine.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- Core stream fusion functionality for text.

module Data.Text.Lazy.Fusion
    (
      stream
    , unstream
    , unstreamChunks
    ) where

import Data.Text.Fusion.Internal
import Data.Text.Lazy.Internal
import qualified Data.Text.Internal as I
import qualified Data.Text.Array as A
import Data.Text.UnsafeChar (unsafeWrite)
import Data.Text.Unsafe (iter)

default(Int)

-- | /O(n)/ Convert a 'Text' into a 'Stream Char'.
stream :: Text -> Stream Char
stream text = Stream next (text :!: 0) 4
  where
    next (Empty :!: _) = Done
    next (txt@(Chunk t@(I.Text _ _ len) ts) :!: i)
        | i >= len  = next (ts :!: 0)
        | otherwise = Yield c (txt :!: i+d)
        where (c,d) = iter t i
{-# INLINE [0] stream #-}

-- | /O(n)/ Convert a 'Stream Char' into a 'Text', using the given
-- chunk size.
unstreamChunks :: Int -> Stream Char -> Text
unstreamChunks chunkSize (Stream next s0 len0)
  | len0 == 0 = Empty
  | otherwise = outer s0
  where
    outer s = case next s of
                Done       -> Empty
                Skip s'    -> outer s'
                Yield x s' -> I.Text arr 0 len `chunk` outer s''
                  where (arr,(s'',len)) = A.run2 fill
                        fill = do a <- A.unsafeNew unknownLength
                                  inner a unknownLength x s' 0
                        unknownLength = 4
    inner marr len x s i
        | i + 1 >= chunkSize = return (marr, (s,i))
        | i + 1 >= len       = do
            let newLen = min (len * 2) chunkSize
            marr' <- A.unsafeNew newLen
            A.copy marr marr'
            inner marr' newLen x s i
        | otherwise =
            case next s of
              Done        -> do i' <- unsafeWrite marr i x
                                return (marr,(s,i'))
              Skip s'     -> inner marr len x s' i
              Yield x' s' -> unsafeWrite marr i x >>= inner marr len x' s' 
{-# INLINE [0] unstreamChunks #-}

-- | /O(n)/ Convert a 'Stream Char' into a 'Text', using
-- 'defaultChunkSize'.
unstream :: Stream Char -> Text
unstream = unstreamChunks defaultChunkSize
{-# INLINE [0] unstream #-}

{-# RULES "LAZY STREAM stream/unstream fusion" forall s.
    stream (unstream s) = s #-}
