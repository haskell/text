-- | Take the first 100 lines from the file, and count the number of lines equal
-- to those lines
--
module Data.Text.Benchmarks.Equality
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
benchmark fp = return $ bgroup "Equality"
    [ bench "Text" $
        B.readFile fp >>= evaluate . func . T.lines . T.decodeUtf8
    , bench "LazyText" $
        BL.readFile fp >>= evaluate . func . TL.lines . TL.decodeUtf8
    , bench "ByteString" $
        B.readFile fp >>= evaluate . func . B.lines
    , bench "LazyByteString" $
        BL.readFile fp >>= evaluate . func . BL.lines
    ]

-- | Frequency of most-appearing line (from the first 100 lines)
--
func :: (Eq a) => [a] -> Int
func ls = sum . map (\needle -> length . filter (== needle) $ ls) $ take 100 ls
