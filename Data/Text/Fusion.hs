{-# LANGUAGE BangPatterns, MagicHash #-}

-- |
-- Module      : Data.Text.Fusion
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
-- Text manipulation functions represented as fusible operations over
-- streams.
module Data.Text.Fusion
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

    -- ** Generation and unfolding
    , unfoldrN

    -- * Indexing
    , index
    , findIndex
    , findIndices
    , findIndexOrEnd
    , elemIndex
    , elemIndices
    , count
    ) where

import Prelude (Bool(..), Char, Eq(..), Maybe(..), Monad(..), Int,
                Num(..), Ord(..), String, ($), (++), (&&),
                fromIntegral, otherwise)
import Data.Bits ((.&.), shiftR)
import Data.Char (ord)
import Data.Text.Internal (Text(..))
import Data.Text.UnsafeChar (unsafeChr, unsafeWrite)
import qualified Data.Text.Array as A
import qualified Data.Text.Fusion.Common as S
import Data.Text.Fusion.Internal
import qualified Data.Text.Internal as I
import qualified Data.Text.Encoding.Utf16 as U16
import qualified Prelude as P

default(Int)

-- | /O(n)/ Convert a 'Text' into a 'Stream Char'.
stream :: Text -> Stream Char
stream (Text arr off len) = Stream next off len
    where
      end = off+len
      {-# INLINE next #-}
      next !i
          | i >= end                   = Done
          | n >= 0xD800 && n <= 0xDBFF = Yield (U16.chr2 n n2) (i + 2)
          | otherwise                  = Yield (unsafeChr n) (i + 1)
          where
            n  = A.unsafeIndex arr i
            n2 = A.unsafeIndex arr (i + 1)
{-# INLINE [0] stream #-}

-- | /O(n)/ Convert a 'Text' into a 'Stream Char', but iterate
-- backwards.
reverseStream :: Text -> Stream Char
reverseStream (Text arr off len) = Stream next (off+len-1) len
    where
      {-# INLINE next #-}
      next !i
          | i < off                    = Done
          | n >= 0xDC00 && n <= 0xDFFF = Yield (U16.chr2 n2 n) (i - 2)
          | otherwise                  = Yield (unsafeChr n) (i - 1)
          where
            n  = A.unsafeIndex arr i
            n2 = A.unsafeIndex arr (i - 1)
{-# INLINE [0] reverseStream #-}

-- | /O(n)/ Convert a 'Stream Char' into a 'Text'.
unstream :: Stream Char -> Text
unstream (Stream next0 s0 len)
    | len == 0  = I.empty
    | otherwise = I.textP (P.fst a) 0 (P.snd a)
    where
      a = A.run2 (A.unsafeNew len >>= (\arr -> loop arr len s0 0))
      loop arr !top !s !i
          | i + 1 >= top = case next0 s of
                            Done -> return (arr, i)
                            _    -> do
                              arr' <- A.unsafeNew (top*2)
                              A.copy arr arr' >> loop arr' (top*2) s i
          | otherwise = case next0 s of
               Done       -> return (arr, i)
               Skip s'    -> loop arr top s' i
               Yield x s' -> unsafeWrite arr i x >>= loop arr top s'
{-# INLINE [0] unstream #-}
{-# RULES "STREAM stream/unstream fusion" forall s. stream (unstream s) = s #-}


-- ----------------------------------------------------------------------------
-- * Basic stream functions

length :: Stream Char -> Int
length = S.lengthI
{-# INLINE[0] length #-}

-- | /O(n)/ Reverse the characters of a string.
reverse :: Stream Char -> Text
reverse (Stream next s len0)
    | len0 == 0 = I.empty
    | otherwise = I.textP arr off' len'
  where
    len0' = max len0 4
    (arr, (off', len')) = A.run2 (A.unsafeNew len0' >>= loop s (len0'-1) len0')
    loop !s0 !i !len marr =
        case next s0 of
          Done -> return (marr, (j, len-j))
              where j = i + 1
          Skip s1    -> loop s1 i len marr
          Yield x s1 | i < least -> do
                       let newLen = len * 2
                       marr' <- A.unsafeNew newLen
                       A.unsafeCopy marr 0 marr' (newLen-len) len
                       write s1 (len+i) newLen marr'
                     | otherwise -> write s1 i len marr
            where n = ord x
                  least | n < 0x10000 = 0
                        | otherwise   = 1
                  m = n - 0x10000
                  lo = fromIntegral $ (m `shiftR` 10) + 0xD800
                  hi = fromIntegral $ (m .&. 0x3FF) + 0xDC00
                  write t j l mar
                      | n < 0x10000 = do
                          A.unsafeWrite mar j (fromIntegral n)
                          loop t (j-1) l mar
                      | otherwise = do
                          A.unsafeWrite mar (j-1) lo
                          A.unsafeWrite mar j hi
                          loop t (j-2) l mar
{-# INLINE [0] reverse #-}

-- | /O(n)/ Perform the equivalent of 'scanr' over a list, only with
-- the input and result reversed.
reverseScanr :: (Char -> Char -> Char) -> Char -> Stream Char -> Stream Char
reverseScanr f z0 (Stream next0 s0 len) = Stream next (S1 :!: z0 :!: s0) (len+1) -- HINT maybe too low
  where
    {-# INLINE next #-}
    next (S1 :!: z :!: s) = Yield z (S2 :!: z :!: s)
    next (S2 :!: z :!: s) = case next0 s of
                              Yield x s' -> let !x' = f x z
                                            in Yield x' (S2 :!: x' :!: s')
                              Skip s'    -> Skip (S2 :!: z :!: s')
                              Done       -> Done
{-# INLINE reverseScanr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a stream from a seed
-- value. However, the length of the result is limited by the
-- first argument to 'unfoldrN'. This function is more efficient than
-- 'unfoldr' when the length of the result is known.
unfoldrN :: Int -> (a -> Maybe (Char,a)) -> a -> Stream Char
unfoldrN n = S.unfoldrNI n
{-# INLINE [0] unfoldrN #-}

-------------------------------------------------------------------------------
-- ** Indexing streams

-- | /O(1)/ stream index (subscript) operator, starting from 0.
index :: Stream Char -> Int -> Char
index (Stream next s0 _len) n0
  | n0 < 0    = streamError "index" "Negative index"
  | otherwise = loop_index n0 s0
  where
    loop_index !n !s = case next s of
      Done                   -> streamError "index" "Index too large"
      Skip    s'             -> loop_index  n    s'
      Yield x s' | n == 0    -> x
                 | otherwise -> loop_index (n-1) s'
{-# INLINE [0] index #-}

-- | The 'findIndex' function takes a predicate and a stream and
-- returns the index of the first element in the stream
-- satisfying the predicate.
findIndex :: (Char -> Bool) -> Stream Char -> Maybe Int
findIndex p s = case findIndices p s of
                  (i:_) -> Just i
                  _     -> Nothing
{-# INLINE [0] findIndex #-}

-- | The 'findIndices' function takes a predicate and a stream and
-- returns all indices of the elements in the stream
-- satisfying the predicate.
findIndices :: (Char -> Bool) -> Stream Char -> [Int]
findIndices p (Stream next s0 _len) = loop_findIndex 0 s0
  where
    loop_findIndex !i !s = case next s of
      Done                   -> []
      Skip    s'             -> loop_findIndex i     s' -- hmm. not caught by QC
      Yield x s' | p x       -> i : loop_findIndex (i+1) s'
                 | otherwise -> loop_findIndex (i+1) s'
{-# INLINE [0] findIndices #-}

-- | The 'findIndexOrEnd' function takes a predicate and a stream and
-- returns the index of the first element in the stream
-- satisfying the predicate.
findIndexOrEnd :: (Char -> Bool) -> Stream Char -> Int
findIndexOrEnd p (Stream next s0 _len) = loop_findIndex 0 s0
  where
    loop_findIndex !i !s = case next s of
      Done                   -> i
      Skip    s'             -> loop_findIndex i     s' -- hmm. not caught by QC
      Yield x s' | p x       -> i
                 | otherwise -> loop_findIndex (i+1) s'
{-# INLINE [0] findIndexOrEnd #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given stream which is equal to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: Char -> Stream Char -> Maybe Int
elemIndex a s = case elemIndices a s of
                  (i:_) -> Just i
                  _     -> Nothing
{-# INLINE [0] elemIndex #-}

-- | /O(n)/ The 'elemIndices' function returns the index of every
-- element in the given stream which is equal to the query element.
elemIndices :: Char -> Stream Char -> [Int]
elemIndices a (Stream next s0 _len) = loop 0 s0
  where
    loop !i !s = case next s of
      Done                   -> []
      Skip    s'             -> loop i s'
      Yield x s' | a == x    -> i : loop (i+1) s'
                 | otherwise -> loop (i+1) s'
{-# INLINE [0] elemIndices #-}

-- | /O(n)/ The 'count' function returns the number of times the query
-- element appears in the given stream.
count :: Char -> Stream Char -> Int
count a (Stream next s0 _len) = loop 0 s0
  where
    loop !i !s = case next s of
      Done                   -> i
      Skip    s'             -> loop i s'
      Yield x s' | a == x    -> loop (i+1) s'
                 | otherwise -> loop i s'
{-# INLINE [0] count #-}

streamError :: String -> String -> a
streamError func msg = P.error $ "Data.Text.Fusion." ++ func ++ ": " ++ msg
