-- | A word frequence count program
--
module Data.Text.Benchmarks.WordCount
    ( benchmark
    ) where

import Control.Exception (evaluate)
import Criterion (Benchmark, bench)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

benchmark :: FilePath -> IO Benchmark
benchmark fp = return $ bench "WordCount" $ do
    t <- T.readFile fp
    evaluate $ M.size $ wordCount t

wordCount :: T.Text -> Map T.Text Int
wordCount =
    foldl' (\m k -> M.insertWith (+) k 1 m) M.empty . map T.toLower . T.words
