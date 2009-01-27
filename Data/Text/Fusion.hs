{-# LANGUAGE ExistentialQuantification, BangPatterns, MagicHash #-}

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

    -- * Basic interface
    , cons
    , snoc
    , append
    , uncons
    , head
    , tail
    , last
    , init
    , null
    , length
    , eq

    -- * Transformations
    , map
    , intercalate
    , intersperse
    , reverse

    -- * Folds
    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1

    -- ** Special folds
    , concat
    , concatMap
    , any
    , all
    , maximum
    , minimum

    -- * Construction
    , replicate
    , unfoldr
    , unfoldrN

    -- * Substrings
    -- ** Breaking strings
    , take
    , drop
    , takeWhile
    , dropWhile

    -- * Searching
    , elem
    , filter

    -- * Indexing
    , find
    , index
    , findIndex
    , elemIndex

    -- * Zipping and unzipping
    , zipWith
    ) where

import Prelude (Bool(..), Char, Either(..), Eq(..), Maybe(..), Monad(..),
                Num(..), Ord(..), String, ($), (++), (.), (&&), error,
                fromIntegral, fst, otherwise, snd)
import Data.Char (ord)
import Control.Monad (liftM2)
import Control.Monad.ST (runST, ST)
import Data.Bits (shiftR, (.&.))
import qualified Data.List as L
import Data.Word (Word16)
import GHC.Exts (Int(..), (+#))
import Data.Text.Internal (Text(..), empty)
import Data.Text.UnsafeChar (unsafeChr)
import qualified Data.Text.Array as A
import qualified Data.Text.Utf16 as U16

default(Int)

infixl 2 :!:
data PairS a b = !a :!: !b

data Switch = S1 | S2

data Stream a = forall s. Stream (s -> Step s a) !s {-# UNPACK #-}!Int

data Step s a = Done
              | Skip !s
              | Yield !a !s

-- | /O(n)/ Convert a Text into a Stream Char.
stream :: Text -> Stream Char
stream (Text arr off len) = Stream next off len
    where
      end = off+len
      {-# INLINE next #-}
      next !i
          | i >= end = Done
          | n >= 0xD800 && n <= 0xDBFF = Yield (U16.chr2 n n2) (i + 2)
          | otherwise = Yield (unsafeChr n) (i + 1)
          where
            n  = A.unsafeIndex arr i
            n2 = A.unsafeIndex arr (i + 1)
{-# INLINE [0] stream #-}

-- | /O(n)/ Convert a Stream Char into a Text.
unstream :: Stream Char -> Text
unstream (Stream next0 s0 len) = Text (fst a) 0 (snd a)
    where
      a :: ((A.Array Word16),Int)
      a = runST ((A.unsafeNew len :: ST s (A.MArray s Word16))
                 >>= (\arr -> loop arr 0 len s0))
      loop arr !i !top !s
          | i + 1 > top = do arr' <- A.unsafeNew (top*2)
                             case next0 s of
                               Done -> liftM2 (,) (A.unsafeFreeze arr) (return i)
                               _    -> copy arr arr' >> loop arr' i (top*2) s
          | otherwise = case next0 s of
               Done       -> liftM2 (,) (A.unsafeFreeze arr) (return i)
               Skip s'    -> loop arr i top s'
               Yield x s'
                   | n < 0x10000 -> do
                        A.unsafeWrite arr i (fromIntegral n :: Word16)
                        loop arr (i+1) top s'
                   | otherwise   -> do
                        A.unsafeWrite arr i       l
                        A.unsafeWrite arr (i + 1) r
                        loop arr (i+2) top s'
                   where
                     n :: Int
                     n = ord x
                     m :: Int
                     m = n - 0x10000
                     l :: Word16
                     l = fromIntegral $ (shiftR m 10) + (0xD800 :: Int)
                     r :: Word16
                     r = fromIntegral $ (m .&. (0x3FF :: Int)) + (0xDC00 :: Int)
{-# INLINE [0] unstream #-}
{-# RULES "STREAM stream/unstream fusion" forall s. stream (unstream s) = s #-}


copy :: A.MArray s Word16 -> A.MArray s Word16 -> ST s ()
copy src dest = copy_loop 0
    where
      len = A.length src
      copy_loop i
          | i > len   = return ()
          | otherwise = do A.unsafeRead src i >>= A.unsafeWrite dest i
                           copy_loop (i+1)

-- | /O(n)/ Determines if two streams are equal.
eq :: Ord a => Stream a -> Stream a -> Bool
eq (Stream next1 s1 _) (Stream next2 s2 _) = cmp (next1 s1) (next2 s2)
    where
      cmp Done Done = True
      cmp Done _    = False
      cmp _    Done = False
      cmp (Skip s1')     (Skip s2')     = cmp (next1 s1') (next2 s2')
      cmp (Skip s1')     x2             = cmp (next1 s1') x2
      cmp x1             (Skip s2')     = cmp x1          (next2 s2')
      cmp (Yield x1 s1') (Yield x2 s2') = x1 == x2 &&
                                          cmp (next1 s1') (next2 s2')
{-# SPECIALISE eq :: Stream Char -> Stream Char -> Bool #-}

internalError :: String -> a
internalError func = error $ "Data.Text.Fusion." ++ func ++ ": internal error"

-- ----------------------------------------------------------------------------
-- * Basic stream functions

-- | /O(n)/ Adds a character to the front of a Stream Char.
cons :: Char -> Stream Char -> Stream Char
cons w (Stream next0 s0 len) = Stream next (S2 :!: s0) (len+2)
    where
      {-# INLINE next #-}
      next (S2 :!: s) = Yield w (S1 :!: s)
      next (S1 :!: s) = case next0 s of
                          Done -> Done
                          Skip s' -> Skip (S1 :!: s')
                          Yield x s' -> Yield x (S1 :!: s')
{-# INLINE [0] cons #-}

-- | /O(n)/ Adds a character to the end of a stream.
snoc :: Stream Char -> Char -> Stream Char
snoc (Stream next0 xs0 len) w = Stream next (Just xs0) (len+2)
  where
    {-# INLINE next #-}
    next (Just xs) = case next0 xs of
      Done        -> Yield w Nothing
      Skip xs'    -> Skip    (Just xs')
      Yield x xs' -> Yield x (Just xs')
    next Nothing = Done
{-# INLINE [0] snoc #-}

-- | /O(n)/ Appends one Stream to the other.
append :: Stream Char -> Stream Char -> Stream Char
append (Stream next0 s01 len1) (Stream next1 s02 len2) =
    Stream next (Left s01) (len1 + len2)
    where
      {-# INLINE next #-}
      next (Left s1) = case next0 s1 of
                         Done        -> Skip    (Right s02)
                         Skip s1'    -> Skip    (Left s1')
                         Yield x s1' -> Yield x (Left s1')
      next (Right s2) = case next1 s2 of
                          Done        -> Done
                          Skip s2'    -> Skip    (Right s2')
                          Yield x s2' -> Yield x (Right s2')
{-# INLINE [0] append #-}

-- | /O(1)/ Returns the first character of a Text, which must be non-empty.
-- Subject to array fusion.
head :: Stream Char -> Char
head (Stream next s0 _len) = loop_head s0
    where
      loop_head !s = case next s of
                      Yield x _ -> x
                      Skip s' -> loop_head s'
                      Done -> error "head: Empty list"
{-# INLINE [0] head #-}

-- | /O(1)/ Returns the first character and remainder of a 'Stream
-- Char', or 'Nothing' if empty.  Subject to array fusion.
uncons :: Stream Char -> Maybe (Char, Stream Char)
uncons (Stream next s0 len) = loop_uncons s0
    where
      loop_uncons !s = case next s of
                         Yield x s1 -> Just (x, Stream next s1 (len-1))
                         Skip s'    -> loop_uncons s'
                         Done       -> Nothing
{-# INLINE [0] uncons #-}

-- | /O(n)/ Returns the last character of a 'Stream Char', which must
-- be non-empty.
last :: Stream Char -> Char
last (Stream next s0 _len) = loop0_last s0
    where
      loop0_last !s = case next s of
                        Done       -> error "last: Empty list"
                        Skip s'    -> loop0_last  s'
                        Yield x s' -> loop_last x s'
      loop_last !x !s = case next s of
                         Done        -> x
                         Skip s'     -> loop_last x  s'
                         Yield x' s' -> loop_last x' s'
{-# INLINE[0] last #-}

-- | /O(1)/ Returns all characters after the head of a Stream Char, which must
-- be non-empty.
tail :: Stream Char -> Stream Char
tail (Stream next0 s0 len) = Stream next (False :!: s0) (len-1)
    where
      {-# INLINE next #-}
      next (False :!: s) = case next0 s of
                          Done -> error "tail"
                          Skip s' -> Skip (False :!: s')
                          Yield _ s' -> Skip (True :!: s')
      next (True :!: s) = case next0 s of
                          Done -> Done
                          Skip s' -> Skip (True :!: s')
                          Yield x s' -> Yield x (True :!: s')
{-# INLINE [0] tail #-}


-- | /O(1)/ Returns all but the last character of a Stream Char, which
-- must be non-empty.
init :: Stream Char -> Stream Char
init (Stream next0 s0 len) = Stream next (Nothing :!: s0) (len-1)
    where
      {-# INLINE next #-}
      next (Nothing :!: s) = case next0 s of
                               Done       -> errorEmptyList "init"
                               Skip s'    -> Skip (Nothing :!: s')
                               Yield x s' -> Skip (Just x  :!: s')
      next (Just x :!: s)  = case next0 s of
                               Done        -> Done
                               Skip s'     -> Skip    (Just x  :!: s')
                               Yield x' s' -> Yield x (Just x' :!: s')
{-# INLINE [0] init #-}

-- | /O(1)/ Tests whether a Stream Char is empty or not.
null :: Stream Char -> Bool
null (Stream next s0 _len) = loop_null s0
    where
      loop_null !s = case next s of
                       Done      -> True
                       Yield _ _ -> False
                       Skip s'   -> loop_null s'
{-# INLINE[0] null #-}

-- | /O(n)/ Returns the number of characters in a text.
length :: Stream Char -> Int
length (Stream next s0 _len) = loop_length 0# s0
    where

      loop_length z# s  = case next s of
                            Done       -> (I# z#)
                            Skip    s' -> loop_length z# s'
                            Yield _ s' -> loop_length (z# +# 1#) s'
{-# INLINE[0] length #-}

-- ----------------------------------------------------------------------------
-- * Stream transformations

-- | /O(n)/ 'map' @f @xs is the Stream Char obtained by applying @f@ to each element of
-- @xs@.
map :: (Char -> Char) -> Stream Char -> Stream Char
map f (Stream next0 s0 len) = Stream next s0 len
    where
      {-# INLINE next #-}
      next !s = case next0 s of
                  Done       -> Done
                  Skip s'    -> Skip s'
                  Yield x s' -> Yield (f x) s'
{-# INLINE [0] map #-}

{-#
  RULES "STREAM map/map fusion" forall f g s.
     map f (map g s) = map (\x -> f (g x)) s
 #-}

-- | /O(n)/ Take a character and place it between each of the
-- characters of a 'Stream Char'.
intersperse :: Char -> Stream Char -> Stream Char
intersperse c (Stream next0 s0 len) = Stream next (s0 :!: Nothing :!: S1) len
    where
      {-# INLINE next #-}
      next (s :!: Nothing :!: S1) = case next0 s of
        Done       -> Done
        Skip s'    -> Skip (s' :!: Nothing :!: S1)
        Yield x s' -> Skip (s' :!: Just x :!: S1)
      next (s :!: Just x :!: S1)  = Yield x (s :!: Nothing :!: S2)
      next (s :!: Nothing :!: S2) = case next0 s of
        Done       -> Done
        Skip s'    -> Skip    (s' :!: Nothing :!: S2)
        Yield x s' -> Yield c (s' :!: Just x :!: S1)
      next _ = internalError "intersperse"
{-# INLINE [0] intersperse #-}

-- | /O(n)/ Reverse the characters of a string.
reverse :: Stream Char -> Text
reverse (Stream next s len) = Text (A.run (A.unsafeNew len >>= fill)) 0 len
  where
    fill marr = loop s len
      where
        loop !s0 !i = case next s0 of
                        Done       -> return marr
                        Skip s1    -> loop s1 i
                        Yield x s1 -> do
                          let i' = i - 1
                              x' = fromIntegral (ord x) :: Word16
                          A.unsafeWrite marr i' x'
                          loop s1 i'
{-# INLINE [0] reverse #-}

-- ----------------------------------------------------------------------------
-- * Reducing Streams (folds)

-- | foldl, applied to a binary operator, a starting value (typically the
-- left-identity of the operator), and a Stream, reduces the Stream using the
-- binary operator, from left to right.
foldl :: (b -> Char -> b) -> b -> Stream Char -> b
foldl f z0 (Stream next s0 _len) = loop_foldl z0 s0
    where
      loop_foldl z !s = case next s of
                          Done -> z
                          Skip s' -> loop_foldl z s'
                          Yield x s' -> loop_foldl (f z x) s'
{-# INLINE [0] foldl #-}

-- | A strict version of foldl.
foldl' :: (b -> Char -> b) -> b -> Stream Char -> b
foldl' f z0 (Stream next s0 _len) = loop_foldl' z0 s0
    where
      loop_foldl' !z !s = case next s of
                            Done -> z
                            Skip s' -> loop_foldl' z s'
                            Yield x s' -> loop_foldl' (f z x) s'
{-# INLINE [0] foldl' #-}

-- | foldl1 is a variant of foldl that has no starting value argument,
-- and thus must be applied to non-empty Streams.
foldl1 :: (Char -> Char -> Char) -> Stream Char -> Char
foldl1 f (Stream next s0 _len) = loop0_foldl1 s0
    where
      loop0_foldl1 !s = case next s of
                          Skip s' -> loop0_foldl1 s'
                          Yield x s' -> loop_foldl1 x s'
                          Done -> errorEmptyList "foldl1"
      loop_foldl1 z !s = case next s of
                           Done -> z
                           Skip s' -> loop_foldl1 z s'
                           Yield x s' -> loop_foldl1 (f z x) s'
{-# INLINE [0] foldl1 #-}

-- | A strict version of foldl1.
foldl1' :: (Char -> Char -> Char) -> Stream Char -> Char
foldl1' f (Stream next s0 _len) = loop0_foldl1' s0
    where
      loop0_foldl1' !s = case next s of
                           Skip s' -> loop0_foldl1' s'
                           Yield x s' -> loop_foldl1' x s'
                           Done -> errorEmptyList "foldl1"
      loop_foldl1' !z !s = case next s of
                             Done -> z
                             Skip s' -> loop_foldl1' z s'
                             Yield x s' -> loop_foldl1' (f z x) s'
{-# INLINE [0] foldl1' #-}

-- | 'foldr', applied to a binary operator, a starting value (typically the
-- right-identity of the operator), and a stream, reduces the stream using the
-- binary operator, from right to left.
foldr :: (Char -> b -> b) -> b -> Stream Char -> b
foldr f z (Stream next s0 _len) = loop_foldr s0
    where
      loop_foldr !s = case next s of
                        Done -> z
                        Skip s' -> loop_foldr s'
                        Yield x s' -> f x (loop_foldr s')
{-# INLINE [0] foldr #-}

-- | foldr1 is a variant of 'foldr' that has no starting value argument,
-- and thust must be applied to non-empty streams.
-- Subject to array fusion.
foldr1 :: (Char -> Char -> Char) -> Stream Char -> Char
foldr1 f (Stream next s0 _len) = loop0_foldr1 s0
  where
    loop0_foldr1 !s = case next s of
      Done       -> error "foldr1"
      Skip    s' -> loop0_foldr1  s'
      Yield x s' -> loop_foldr1 x s'

    loop_foldr1 x !s = case next s of
      Done        -> x
      Skip     s' -> loop_foldr1 x s'
      Yield x' s' -> f x (loop_foldr1 x' s')
{-# INLINE [0] foldr1 #-}

intercalate :: Stream Char -> [Stream Char] -> Stream Char
intercalate s = concat . (L.intersperse s)
{-# INLINE [0] intercalate #-}

-- ----------------------------------------------------------------------------
-- ** Special folds

-- | /O(n)/ Concatenate a list of streams. Subject to array fusion.
concat :: [Stream Char] -> Stream Char
concat = L.foldr append (Stream next Done 0)
    where
      next Done = Done
      next _    = internalError "concat"

-- | Map a function over a stream that results in a stream and concatenate the
-- results.
concatMap :: (Char -> Stream Char) -> Stream Char -> Stream Char
concatMap f = foldr (append . f) (stream empty)

-- | /O(n)/ any @p @xs determines if any character in the stream
-- @xs@ satisifes the predicate @p@.
any :: (Char -> Bool) -> Stream Char -> Bool
any p (Stream next0 s0 _len) = loop_any s0
    where
      loop_any !s = case next0 s of
                      Done                   -> False
                      Skip s'                -> loop_any s'
                      Yield x s' | p x       -> True
                                 | otherwise -> loop_any s'

-- | /O(n)/ all @p @xs determines if all characters in the 'Text'
-- @xs@ satisify the predicate @p@.
all :: (Char -> Bool) -> Stream Char -> Bool
all p (Stream next0 s0 _len) = loop_all s0
    where
      loop_all !s = case next0 s of
                      Done                   -> True
                      Skip s'                -> loop_all s'
                      Yield x s' | p x       -> loop_all s'
                                 | otherwise -> False

-- | /O(n)/ maximum returns the maximum value from a stream, which must be
-- non-empty.
maximum :: Stream Char -> Char
maximum (Stream next0 s0 _len) = loop0_maximum s0
    where
      loop0_maximum !s   = case next0 s of
                             Done       -> errorEmptyList "maximum"
                             Skip s'    -> loop0_maximum s'
                             Yield x s' -> loop_maximum x s'
      loop_maximum !z !s = case next0 s of
                             Done            -> z
                             Skip s'         -> loop_maximum z s'
                             Yield x s'
                                 | x > z     -> loop_maximum x s'
                                 | otherwise -> loop_maximum z s'

-- | /O(n)/ minimum returns the minimum value from a 'Text', which must be
-- non-empty.
minimum :: Stream Char -> Char
minimum (Stream next0 s0 _len) = loop0_minimum s0
    where
      loop0_minimum !s   = case next0 s of
                             Done       -> errorEmptyList "minimum"
                             Skip s'    -> loop0_minimum s'
                             Yield x s' -> loop_minimum x s'
      loop_minimum !z !s = case next0 s of
                             Done            -> z
                             Skip s'         -> loop_minimum z s'
                             Yield x s'
                                 | x < z     -> loop_minimum x s'
                                 | otherwise -> loop_minimum z s'




-- -----------------------------------------------------------------------------
-- * Building streams

-- -----------------------------------------------------------------------------
-- ** Generating and unfolding streams

replicate :: Int -> Char -> Stream Char
replicate n c = Stream next 0 n
  where
    {-# INLINE next #-}
    next i | i >= n    = Done
           | otherwise = Yield c (i + 1)
{-# INLINE [0] replicate #-}

-- | /O(n)/, where @n@ is the length of the result. The unfoldr function
-- is analogous to the List 'unfoldr'. unfoldr builds a stream
-- from a seed value. The function takes the element and returns
-- Nothing if it is done producing the stream or returns Just
-- (a,b), in which case, a is the next Char in the string, and b is
-- the seed value for further production.
unfoldr :: (a -> Maybe (Char,a)) -> a -> Stream Char
unfoldr f s0 = Stream next s0 1
    where
      {-# INLINE next #-}
      next !s = case f s of
                 Nothing      -> Done
                 Just (w, s') -> Yield w s'
{-# INLINE [0] unfoldr #-}

-- | O(n) Like unfoldr, unfoldrN builds a stream from a seed
-- value. However, the length of the result should be limited by the
-- first argument to unfoldrN. This function is more efficient than
-- unfoldr when the maximum length of the result and correct,
-- otherwise its complexity performance is similar to 'unfoldr'
unfoldrN :: Int -> (a -> Maybe (Char,a)) -> a -> Stream Char
unfoldrN n f s0 = Stream next (0 :!: s0) (n*2)
    where
      {-# INLINE next #-}
      next (z :!: s) = case f s of
          Nothing                  -> Done
          Just (w, s') | z >= n    -> Done
                       | otherwise -> Yield w ((z + 1) :!: s')
-------------------------------------------------------------------------------
--  * Substreams

-- | /O(n)/ take n, applied to a stream, returns the prefix of the
-- stream of length @n@, or the stream itself if @n@ is greater than the
-- length of the stream.
take :: Int -> Stream Char -> Stream Char
take n0 (Stream next0 s0 len) = Stream next (n0 :!: s0) len
    where
      {-# INLINE next #-}
      next (n :!: s) | n <= 0    = Done
                     | otherwise = case next0 s of
                                     Done -> Done
                                     Skip s' -> Skip (n :!: s')
                                     Yield x s' -> Yield x ((n-1) :!: s')
{-# INLINE [0] take #-}

-- | /O(n)/ drop n, applied to a stream, returns the suffix of the
-- stream of length @n@, or the empty stream if @n@ is greater than the
-- length of the stream.
drop :: Int -> Stream Char -> Stream Char
drop n0 (Stream next0 s0 len) = Stream next (Just ((max 0 n0)) :!: s0) (len - n0)
  where
    {-# INLINE next #-}
    next (Just !n :!: s)
      | n == 0    = Skip (Nothing :!: s)
      | otherwise = case next0 s of
          Done       -> Done
          Skip    s' -> Skip (Just n    :!: s')
          Yield _ s' -> Skip (Just (n-1) :!: s')
    next (Nothing :!: s) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (Nothing :!: s')
      Yield x s' -> Yield x (Nothing :!: s')
{-# INLINE [0] drop #-}

-- | takeWhile, applied to a predicate @p@ and a stream, returns the
-- longest prefix (possibly empty) of elements that satisfy p.
takeWhile :: (Char -> Bool) -> Stream Char -> Stream Char
takeWhile p (Stream next0 s0 len) = Stream next s0 len
    where
      {-# INLINE next #-}
      next !s = case next0 s of
                  Done    -> Done
                  Skip s' -> Skip s'
                  Yield x s' | p x       -> Yield x s'
                             | otherwise -> Done
{-# INLINE [0] takeWhile #-}

-- | dropWhile @p @xs returns the suffix remaining after takeWhile @p @xs.
dropWhile :: (Char -> Bool) -> Stream Char -> Stream Char
dropWhile p (Stream next0 s0 len) = Stream next (S1 :!: s0) len
    where
    {-# INLINE next #-}
    next (S1 :!: s)  = case next0 s of
      Done                   -> Done
      Skip    s'             -> Skip    (S1 :!: s')
      Yield x s' | p x       -> Skip    (S1 :!: s')
                 | otherwise -> Yield x (S2 :!: s')
    next (S2 :!: s) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (S2 :!: s')
      Yield x s' -> Yield x (S2 :!: s')
{-# INLINE [0] dropWhile #-}

-- ----------------------------------------------------------------------------
-- * Searching

-------------------------------------------------------------------------------
-- ** Searching by equality

-- | /O(n)/ elem is the stream membership predicate.
elem :: Char -> Stream Char -> Bool
elem w (Stream next s0 _len) = loop_elem s0
    where
      loop_elem !s = case next s of
                       Done -> False
                       Skip s' -> loop_elem s'
                       Yield x s' | x == w -> True
                                  | otherwise -> loop_elem s'
{-# INLINE [0] elem #-}

-------------------------------------------------------------------------------
-- ** Searching with a predicate

-- | /O(n)/ The 'find' function takes a predicate and a stream,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.

find :: (Char -> Bool) -> Stream Char -> Maybe Char
find p (Stream next s0 _len) = loop_find s0
    where
      loop_find !s = case next s of
                       Done -> Nothing
                       Skip s' -> loop_find s'
                       Yield x s' | p x -> Just x
                                  | otherwise -> loop_find s'
{-# INLINE [0] find #-}

-- | /O(n)/ 'filter', applied to a predicate and a stream,
-- returns a stream containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> Stream Char -> Stream Char
filter p (Stream next0 s0 len) = Stream next s0 len
  where
    {-# INLINE next #-}
    next !s = case next0 s of
                Done                   -> Done
                Skip    s'             -> Skip    s'
                Yield x s' | p x       -> Yield x s'
                           | otherwise -> Skip    s'
{-# INLINE [0] filter #-}

{-# RULES
  "Stream filter/filter fusion" forall p q s.
  filter p (filter q s) = filter (\x -> q x && p x) s
  #-}

-------------------------------------------------------------------------------
-- ** Indexing streams

-- | /O(1)/ stream index (subscript) operator, starting from 0.
index :: Stream Char -> Int -> Char
index (Stream next s0 _len) n0
  | n0 < 0    = error "Stream.(!!): negative index"
  | otherwise = loop_index n0 s0
  where
    loop_index !n !s = case next s of
      Done                   -> error "Stream.(!!): index too large"
      Skip    s'             -> loop_index  n    s'
      Yield x s' | n == 0    -> x
                 | otherwise -> loop_index (n-1) s'
{-# INLINE [0] index #-}

-- | The 'findIndex' function takes a predicate and a stream and
-- returns the index of the first element in the stream
-- satisfying the predicate.
findIndex :: (Char -> Bool) -> Stream Char -> Maybe Int
findIndex p (Stream next s0 _len) = loop_findIndex 0 s0
  where
    loop_findIndex !i !s = case next s of
      Done                   -> Nothing
      Skip    s'             -> loop_findIndex i     s' -- hmm. not caught by QC
      Yield x s' | p x       -> Just i
                 | otherwise -> loop_findIndex (i+1) s'
{-# INLINE [0] findIndex #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given stream which is equal to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: Char -> Stream Char -> Maybe Int
elemIndex a (Stream next s0 _len) = loop_elemIndex 0 s0
  where
    loop_elemIndex !i !s = case next s of
      Done                   -> Nothing
      Skip    s'             -> loop_elemIndex i     s'
      Yield x s' | a == x    -> Just i
                 | otherwise -> loop_elemIndex (i+1) s'
{-# INLINE [0] elemIndex #-}

-------------------------------------------------------------------------------
-- * Zipping

-- | zipWith generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.
zipWith :: (Char -> Char -> Char) -> Stream Char -> Stream Char -> Stream Char
zipWith f (Stream next0 sa0 len1) (Stream next1 sb0 len2) = Stream next (sa0 :!: sb0 :!: Nothing) (min len1 len2)
    where
      {-# INLINE next #-}
      next (sa :!: sb :!: Nothing) = case next0 sa of
                                       Done -> Done
                                       Skip sa' -> Skip (sa' :!: sb :!: Nothing)
                                       Yield a sa' -> Skip (sa' :!: sb :!: Just a)

      next (sa' :!: sb :!: Just a) = case next1 sb of
                                       Done -> Done
                                       Skip sb' -> Skip (sa' :!: sb' :!: Just a)
                                       Yield b sb' -> Yield (f a b) (sa' :!: sb' :!: Nothing)
{-# INLINE [0] zipWith #-}

errorEmptyList :: String -> a
errorEmptyList fun = error ("Data.Text.Fusion." ++ fun ++ ": empty list")
