{-# LANGUAGE BangPatterns #-}

module SlowFunctions
    (
      indices
    , split
    ) where

import qualified Data.Text as T
import Data.Text.Internal (Text(..))
import Data.Text.Unsafe (iter_, unsafeHead, unsafeTail)

indices :: Text                -- ^ Substring to search for (@needle@)
        -> Text                -- ^ Text to search in (@haystack@)
        -> [Int]
indices needle@(Text _narr _noff nlen) haystack@(Text harr hoff hlen)
    | T.null needle = []
    | otherwise     = scan 0
  where
    scan i | i >= hlen = []
           | needle `T.isPrefixOf` t = i : scan (i+nlen)
           | otherwise = scan (i+d)
           where t = Text harr (hoff+i) (hlen-i)
                 d = iter_ haystack i

split :: Text                   -- ^ Text to split on
      -> Text                   -- ^ Input text
      -> [Text]
split pat src0
    | T.null pat  = error "split: empty"
    | l == 1      = T.splitBy (== (unsafeHead pat)) src0
    | otherwise   = go src0
  where
    l      = T.length pat
    go src = search 0 src
      where
        search !n !s
            | T.null s             = [src]      -- not found
            | pat `T.isPrefixOf` s = T.take n src : go (T.drop l s)
            | otherwise            = search (n+1) (unsafeTail s)
