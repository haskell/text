{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module      : Data.Text.Compat
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtharper@aftereternity.co.uk,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- A text API for which function names closely resemble those of the
-- list and 'ByteString' types.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.Text.Compat as T

module Data.Text.Compat
    (
    -- * Types
      Text

    -- * Creation and elimination
    , pack
    , unpack
    , singleton
    , empty

    -- * Basic interface
    , cons
    , snoc
    , append
    , uncons
    , head
    , last
    , tail
    , init
    , null
    , length

    -- * Transformations
    , map
    , intercalate
    , intersperse
    , transpose
    , reverse

    -- * Case conversion
    -- $case
    , toCaseFold
    , toLower
    , toUpper

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

    -- ** Scans
    , scanl
    , scanl1
    , scanr
    , scanr1

    -- ** Accumulating maps
    , mapAccumL
    , mapAccumR

    -- ** Generation and unfolding
    , replicate
    , unfoldr
    , unfoldrN

    -- * Substrings

    -- ** Breaking strings
    , take
    , drop
    , takeWhile
    , dropWhile
    , splitAt
    , span
    , break
    , group
    , groupBy
    , inits
    , tails

    -- ** Breaking into many substrings
    , split
    , splitWith
    , breakSubstring

    -- ** Breaking into lines and words
    , lines
    --, lines'
    , words
    , unlines
    , unwords

    -- * Predicates
    , isPrefixOf
    , isSuffixOf
    , isInfixOf

    -- * Searching
    , elem
    , filter
    , find
    , partition

    -- , findSubstring
    
    -- * Indexing
    , index
    , findIndex
    , findIndices
    , elemIndex
    , elemIndices
    , count

    -- * Zipping and unzipping
    , zip
    , zipWith

    -- -* Ordered text
    -- , sort
    ) where

import Data.ByteString (ByteString)
import Data.Text hiding (split)
import Data.Text.Unsafe (unsafeTail)
import Prelude (Char, (+), otherwise)

split :: Char -> Text -> [Text]
split = splitChar
{-# INLINE split #-}

-- | /O(n)/ Break a string on a substring, returning a pair of the
-- part of the string prior to the match, and the rest of the string.
--
-- The following relationship holds:
--
-- > break (==c) l == breakSubstring (singleton c) l
--
-- For example, to tokenise a string, dropping delimiters:
--
-- > tokenise x y = h : if null t then [] else tokenise x (drop (length x) t)
-- >     where (h,t) = breakSubstring x y
--
-- To skip to the first occurence of a string:
--
-- > snd (breakSubstring x y)
--
-- To take the parts of a string before a delimiter:
--
-- > fst (breakSubstring x y)
--
breakSubstring :: Text -- ^ String to search for
               -> Text -- ^ String to search in
               -> (Text,Text) -- ^ Head and tail of string broken at substring

breakSubstring pat src = search 0 src
  where
    search !n !s
        | null s             = (src,empty)      -- not found
        | pat `isPrefixOf` s = (take n src,s)
        | otherwise          = search (n+1) (unsafeTail s)
{-# INLINE breakSubstring #-}
