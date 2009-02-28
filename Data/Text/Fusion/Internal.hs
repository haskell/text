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
    , eq
    ) where

infixl 2 :!:
data PairS a b = !a :!: !b

-- | Allow a function over a stream to switch between two states.
data Switch = S1 | S2

data Step s a = Done
              | Skip !s
              | Yield !a !s

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
eq :: Ord a => Stream a -> Stream a -> Bool
{-# INLINE eq #-}
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
