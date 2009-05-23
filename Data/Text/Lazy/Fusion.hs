-- |
-- Module      : Data.Text.Lazy.Fusion
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtharper@aftereternity.co.uk,
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
    , length
    , unfoldrN
    , index
    , findIndex
    , findIndices
    , elemIndex
    , elemIndices
    , count
    ) where

import Prelude hiding (length)
import qualified Data.Text.Fusion.Common as S
import Data.Text.Fusion.Internal
import Data.Text.Lazy.Internal
import qualified Data.Text.Internal as I
import qualified Data.Text.Array as A
import Data.Text.UnsafeChar (unsafeWrite)
import Data.Text.Unsafe (iter)
import Data.Int (Int64)

default(Int64)

-- | /O(n)/ Convert a 'Text' into a 'Stream Char'.
stream :: Text -> Stream Char
stream text = Stream next (text :!: 0) 4 -- random HINT
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
                                  i <- unsafeWrite a 0 x
                                  inner a unknownLength s' i
                        unknownLength = 4
    inner marr len s i
        | i + 1 >= chunkSize = return (marr, (s,i))
        | i + 1 >= len       = do
            let newLen = min (len * 2) chunkSize
            marr' <- A.unsafeNew newLen
            A.copy marr marr'
            inner marr' newLen s i
        | otherwise =
            case next s of
              Done        -> return (marr,(s,i))
              Skip s'     -> inner marr len s' i
              Yield x s'  -> unsafeWrite marr i x >>= inner marr len s'
{-# INLINE [0] unstreamChunks #-}

-- | /O(n)/ Convert a 'Stream Char' into a 'Text', using
-- 'defaultChunkSize'.
unstream :: Stream Char -> Text
unstream = unstreamChunks defaultChunkSize
{-# INLINE [0] unstream #-}

-- | /O(n)/ Returns the number of characters in a text.
length :: Stream Char -> Int64
length = S.lengthI
{-# INLINE[0] length #-}

{-# RULES "LAZY STREAM stream/unstream fusion" forall s.
    stream (unstream s) = s #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN64' builds a stream from a seed
-- value. However, the length of the result is limited by the
-- first argument to 'unfoldrN64'. This function is more efficient than
-- 'unfoldr' when the length of the result is known.
unfoldrN :: Int64 -> (a -> Maybe (Char,a)) -> a -> Stream Char
unfoldrN n = S.unfoldrNI n
{-# INLINE [0] unfoldrN #-}

-- | /O(n)/ stream index (subscript) operator, starting from 0.
index :: Stream Char -> Int64 -> Char
index = S.indexI
{-# INLINE [0] index #-}

-- | The 'findIndex' function takes a predicate and a stream and
-- returns the index of the first element in the stream
-- satisfying the predicate.
findIndex :: (Char -> Bool) -> Stream Char -> Maybe Int64
findIndex = S.findIndexI
{-# INLINE [0] findIndex #-}

-- | The 'findIndices' function takes a predicate and a stream and
-- returns all indices of the elements in the stream
-- satisfying the predicate.
findIndices :: (Char -> Bool) -> Stream Char -> [Int64]
findIndices = S.findIndicesI
{-# INLINE [0] findIndices #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given stream which is equal to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: Char -> Stream Char -> Maybe Int64
elemIndex = S.elemIndexI
{-# INLINE [0] elemIndex #-}

-- | /O(n)/ The 'elemIndices' function returns the index of every
-- element in the given stream which is equal to the query element.
elemIndices :: Char -> Stream Char -> [Int64]
elemIndices = S.elemIndicesI
{-# INLINE [0] elemIndices #-}

-- | /O(n)/ The 'count' function returns the number of times the query
-- element appears in the given stream.
count :: Char -> Stream Char -> Int64
count = S.countI
{-# INLINE [0] count #-}
