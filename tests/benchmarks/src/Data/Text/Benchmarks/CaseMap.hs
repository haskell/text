-- | This benchmark converts a number of UTF-8 encoded files to uppercase
--
module Data.Text.Benchmarks.CaseMap
    ( benchmark
    ) where

import Criterion (Benchmark, bench)
import System.IO (Handle)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

benchmark :: FilePath -> Handle -> IO Benchmark
benchmark fp sink = return $ bench "CaseMap" $
    B.readFile fp >>= B.hPutStr sink . T.encodeUtf8 . T.toUpper . T.decodeUtf8
