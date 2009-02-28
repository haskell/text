-- |
-- Module      : Data.Text.Lazy.Fusion
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009,
--               (c) Duncan Coutts 2009
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
    ) where

import Data.Text.Fusion.Internal
import Data.Text.Lazy.Internal
import qualified Data.Text.Internal as I
import qualified Data.Text.Array as A
import qualified Data.Text.Encoding.Utf16 as U16
import Data.Text.UnsafeChar (unsafeChr, unsafeWrite)
import Data.Text.Unsafe (iter)

default(Int)

stream :: Text -> Stream Char
stream text = Stream next (text :!: 0) 4
  where
    next (Empty :!: _) = Done
    next (txt@(Chunk t@(I.Text _ _ len) ts) :!: i)
        | i >= len  = next (ts :!: 0)
        | otherwise = Yield c (txt :!: i+d)
        where (c,d) = iter t i
{-# INLINE [0] stream #-}

unstream :: Stream Char -> Text
unstream (Stream next s0 len0)
  | len0 == 0 = Empty
  | otherwise = outer s0
  where
    outer s = case next s of
                Done    -> Empty
                Skip s' -> outer s'
                Yield x s' -> chunk t (outer s'')
                  where (t,s'') = fill x s'
    fill x s = (I.Text a 0 l,s')
        where (a,(s',l)) = A.run2 (A.unsafeNew initLen >>= (\arr -> inner arr initLen x s 0))
    initLen = 8
    inner marr len x s i
        | i + 1 >= defaultChunkSize = return (marr, (s,i))
        | i + 1 >= len = do
            let newLen = min (len * 2) defaultChunkSize
            marr' <- A.unsafeNew newLen
            A.copy marr marr'
            inner marr' newLen x s i
        | otherwise =
            case next s of
              Done        -> do i' <- unsafeWrite marr i x
                                return (marr,(s,i'))
              Skip s'     -> inner marr len x s i
              Yield x' s' -> unsafeWrite marr i x >>= inner marr len x' s' 
{-# INLINE [0] unstream #-}
{-# RULES "STREAM stream/unstream fusion" forall s. stream (unstream s) = s #-}
