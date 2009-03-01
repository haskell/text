{-# LANGUAGE BangPatterns, ExistentialQuantification #-}
-- |
-- Module      : Data.Text.Fusion.Internal
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

module Data.Text.Fusion.Internal
    (
      PairS(..)
    , Switch(..)
    , Step(..)
    , Stream(..)
    , singleton
    , empty
    , streamList
    , unstreamList
    ) where

infixl 2 :!:
data PairS a b = !a :!: !b
               deriving (Eq, Ord, Read, Show)

-- | Allow a function over a stream to switch between two states.
data Switch = S1 | S2

data Step s a = Done
              | Skip !s
              | Yield !a !s

instance Show a => Show (Step s a)
    where show Done        = "Done"
          show (Skip _)    = "Skip"
          show (Yield x _) = "Yield " ++ show x

instance (Eq a) => Eq (Stream a) where
    (==) = eq

instance (Ord a) => Ord (Stream a) where
    compare = cmp

-- The length hint in a Stream has two roles.  If its value is zero,
-- we trust it, and treat the stream as empty.  Otherwise, we treat it
-- as a hint: it should usually be accurate, so we use it when
-- unstreaming to decide what size array to allocate.  However, the
-- unstreaming functions must be able to cope with the hint being too
-- small or too large.
--
-- The size hint tries to track the UTF-16 code points in a stream,
-- but often counts the number of characters instead.  It can easily
-- undercount if, for instance, a transformed stream contains astral
-- plane characters (those above 0x10000).

data Stream a =
    forall s. Stream
    (s -> Step s a)             -- stepper function
    !s                          -- current state
    {-# UNPACK #-}!Int          -- length hint

singleton :: Char -> Stream Char
singleton c = Stream next False 1
    where next False = Yield c True
          next True  = Done
{-# INLINE singleton #-}

-- | The empty stream.
empty :: Stream a
empty = Stream next () 0
    where next _ = Done
{-# INLINE [0] empty #-}

-- | /O(n)/ Determines if two streams are equal.
eq :: (Eq a) => Stream a -> Stream a -> Bool
eq (Stream next1 s1 _) (Stream next2 s2 _) = loop (next1 s1) (next2 s2)
    where
      loop Done Done                     = True
      loop (Skip s1')     (Skip s2')     = loop (next1 s1') (next2 s2')
      loop (Skip s1')     x2             = loop (next1 s1') x2
      loop x1             (Skip s2')     = loop x1          (next2 s2')
      loop Done _                        = False
      loop _    Done                     = False
      loop (Yield x1 s1') (Yield x2 s2') = x1 == x2 &&
                                           loop (next1 s1') (next2 s2')
{-# INLINE [0] eq #-}
{-# SPECIALISE eq :: Stream Char -> Stream Char -> Bool #-}

cmp :: (Ord a) => Stream a -> Stream a -> Ordering
cmp (Stream next1 s1 _) (Stream next2 s2 _) = loop (next1 s1) (next2 s2)
    where
      loop Done Done                     = EQ
      loop (Skip s1')     (Skip s2')     = loop (next1 s1') (next2 s2')
      loop (Skip s1')     x2             = loop (next1 s1') x2
      loop x1             (Skip s2')     = loop x1          (next2 s2')
      loop Done _                        = LT
      loop _    Done                     = GT
      loop (Yield x1 s1') (Yield x2 s2') =
          case compare x1 x2 of
            EQ    -> loop (next1 s1') (next2 s2')
            other -> other
{-# INLINE [0] cmp #-}
{-# SPECIALISE cmp :: Stream Char -> Stream Char -> Ordering #-}

streamList :: [a] -> Stream a
{-# INLINE streamList #-}
streamList [] = empty
streamList s  = Stream next s unknownLength
    where next []       = Done
          next (x:xs)   = Yield x xs
          unknownLength = 8

unstreamList :: Stream a -> [a]
{-# INLINE unstreamList #-}
unstreamList (Stream next s0 _len) = unfold s0
    where unfold !s = case next s of
                        Done       -> []
                        Skip s'    -> unfold s'
                        Yield x s' -> x : unfold s'
