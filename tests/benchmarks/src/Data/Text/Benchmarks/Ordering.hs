-- | For every 1000th line of a file, check how many lines in the file are
-- lexicographically smaller
--
module Data.Text.Benchmarks.Ordering
    ( benchmark
    ) where

import Control.Exception (evaluate)
import Criterion (Benchmark, bgroup, bench)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

benchmark :: FilePath -> IO Benchmark
benchmark fp = return $ bgroup "Ordering"
    [ bench "ByteString" $ B.readFile fp >>=
        evaluate . numSmaller . B.lines
    , bench "LazyByteString" $ BL.readFile fp >>=
        evaluate . numSmaller . BL.lines

    , bench "Text" $ B.readFile fp >>=
        evaluate . numSmaller . T.lines . T.decodeUtf8
    , bench "TextFusion" $ B.readFile fp >>=
        evaluate . numSmallerFusion . T.lines . T.decodeUtf8
    , bench "LazyText" $ BL.readFile fp >>=
        evaluate . numSmaller . TL.lines . TL.decodeUtf8

    , bench "String" $ readFile fp >>=
        evaluate . numSmaller . lines
    ]

-- | Take every Nth item from a list
--
every :: Int -> [a] -> [a]
every k = go k
  where
    go n (x : xs)
        | n < k     = go (n+1) xs
        | otherwise = x : go 1 xs
    go _ _          = []

-- | Benchmark logic
--
numSmaller :: (Ord a) => [a] -> Int
numSmaller ls = sum . map f $ every 1000 ls
  where
    f x = length . filter ((== GT) . compare x) $ ls

-- | Test a comparison that could be fused: compare (toLower a) (toLower b)
-- Currently, this funcion performs very badly!
--
numSmallerFusion :: [T.Text] -> Int
numSmallerFusion ls = sum . map f $ every 1000 ls
  where
    f x = length . filter ((== GT) . compare (T.toLower x) . T.toLower) $ ls
