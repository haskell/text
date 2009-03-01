{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Text.Lazy
-- Copyright   : (c) Bryan O'Sullivan 2009,
--               (c) Tom Harper 2008-2009,
--               (c) Duncan Coutts 2008
--
-- License     : BSD-style
-- Maintainer  : rtharper@aftereternity.co.uk, bos@serpentine.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- A time and space-efficient implementation of Unicode text using
-- lists of packed arrays.  This representation is suitable for high
-- performance use and for streaming large quantities of data.  It
-- provides a means to manipulate a large body of text without
-- requiring that the entire content be resident in memory.
--
-- Some operations, such as 'concat', 'append', 'reverse' and 'cons',
-- have better complexity than their "Data.Text" equivalents, due to
-- optimisations resulting from the list spine structure. And for
-- other operations lazy 'Text's are usually within a few percent of
-- strict ones, but with better heap usage. For data larger than
-- available memory, or if you have tight memory constraints, this
-- module will be the only option.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.Text.Lazy as B

module Data.Text.Lazy
    (
      Text
    -- * Creation and elimination
    , pack
    , unpack
    , singleton
    , empty

    -- * Basic interface
    , cons
    -- , snoc
    , append
    -- , uncons
    -- , head
    -- , last
    -- , tail
    -- , init
    -- , null
    -- , length

    -- * Transformations
    -- , map
    -- , intercalate
    -- , intersperse
    -- , transpose
    -- , reverse

    -- * Folds
    -- , foldl
    -- , foldl'
    -- , foldl1
    -- , foldl1'
    -- , foldr
    -- , foldr1

    -- ** Special folds
    -- , concat
    -- , concatMap
    -- , any
    -- , all
    -- , maximum
    -- , minimum

    -- * Construction

    -- ** Scans
    -- , scanl
    -- , scanl1
    -- , scanr
    -- , scanr1

    -- ** Accumulating maps
    -- , mapAccumL
    -- , mapAccumR

    -- ** Generation and unfolding
    -- , replicate
    -- , unfoldr
    -- , unfoldrN

    -- * Substrings

    -- ** Breaking strings
    -- , take
    -- , drop
    -- , takeWhile
    -- , dropWhile
    , splitAt
    -- , span
    -- , break
    -- , group
    -- , groupBy
    -- , inits
    -- , tails

    -- ** Breaking into many substrings
    -- , split
    -- , splitWith
    -- , breakSubstring

    -- ** Breaking into lines and words
    -- , lines
    --, lines'
    -- , words
    -- , unlines
    -- , unwords

    -- * Predicates
    -- , isPrefixOf
    -- , isSuffixOf
    -- , isInfixOf

    -- * Searching
    -- , elem
    -- , filter
    -- , find
    -- , partition

    -- , findSubstring
    
    -- * Indexing
    -- , index
    -- , findIndex
    -- , findIndices
    -- , elemIndex
    -- , elemIndices
    -- , count

    -- * Zipping and unzipping
    -- , zipWith

    -- -* Ordered text
    -- , sort
    ) where

import Prelude (Char, Bool(..), Functor(..), Int, Maybe(..), String,
                Eq(..), Ord(..), (++),
                Read(..), Show(..),
                (&&), (||), (+), (-), (.), ($),
                not, return, otherwise)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Fusion as S
import qualified Data.Text.Fusion.Internal as S
import Data.Text.Lazy.Fusion
import Data.Text.Lazy.Internal

instance Eq Text where
    t1 == t2 = stream t1 == stream t2
    {-# INLINE (==) #-}

instance Ord Text where
    compare t1 t2 = compare (stream t1) (stream t2)
    {-# INLINE compare #-}

instance Show Text where
    showsPrec p ps r = showsPrec p (unpack ps) r

instance Read Text where
    readsPrec p str = [(pack x,y) | (x,y) <- readsPrec p str]

instance IsString Text where
    fromString = pack

-- | /O(n)/ Convert a 'String' into a 'Text'.
--
-- This function is subject to array fusion.
pack :: String -> Text
pack s = unstream (S.streamList s)
{-# INLINE [1] pack #-}

-- | /O(n)/ Convert a 'Text' into a 'String'.
-- Subject to array fusion.
unpack :: Text -> String
unpack t = S.unstreamList (stream t)
{-# INLINE [1] unpack #-}

singleton :: Char -> Text
singleton c = Chunk (T.singleton c) Empty
{-# INLINE [1] singleton #-}

{-# RULES
"LAZY TEXT singleton -> fused" [~1] forall c.
    singleton c = unstream (S.singleton c)
"LAZY TEXT singleton -> unfused" [1] forall c.
    unstream (S.singleton c) = singleton c
  #-}

cons :: Char -> Text -> Text
cons c t = Chunk (T.singleton c) t
{-# INLINE [1] cons #-}

{-# RULES
"LAZY TEXT cons -> fused" [~1] forall c t.
    cons c t = unstream (S.cons c (stream t))
"LAZY TEXT cons -> unfused" [1] forall c t.
    unstream (S.cons c (stream t)) = cons c t
 #-}

-- | /O(n\/c)/ Append two 'Text's
append :: Text -> Text -> Text
append xs ys = foldrChunks Chunk ys xs
{-# INLINE append #-}

{-# RULES
"LAZY TEXT append -> fused" [~1] forall t1 t2.
    append t1 t2 = unstream (S.append (stream t1) (stream t2))
"LAZY TEXT append -> unfused" [1] forall t1 t2.
    unstream (S.append (stream t1) (stream t2)) = append t1 t2
 #-}

-- | /O(n)/ 'splitAt' @n t@ returns a pair whose first element is a
-- prefix of @t@ of length @n@, and whose second is the remainder of
-- the string. It is equivalent to @('take' n t, 'drop' n t)@.
splitAt :: Int -> Text -> (Text, Text)
splitAt = loop
  where loop _ Empty      = (empty, empty)
        loop n t | n <= 0 = (empty, t)
        loop n (Chunk t ts)
             | n < len   = let (ts',ts'') = T.splitAt n t
                           in (Chunk ts' Empty, Chunk ts'' Empty)
             | otherwise = let (ts',ts'') = loop (n - len) ts
                           in (Chunk t ts', ts'')
             where len = T.length t
