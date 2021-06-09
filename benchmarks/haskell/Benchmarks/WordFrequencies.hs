-- | A word frequency count using the different string types
--
-- Tested in this benchmark:
--
-- * Splitting into words
--
-- * Converting to lowercase
--
-- * Comparing: Eq/Ord instances
--
module Benchmarks.WordFrequencies
    ( initEnv
    , benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Env = T.Text

initEnv :: FilePath -> IO Env
initEnv fp = do
    t <- T.readFile fp
    return t

benchmark :: Env -> Benchmark
benchmark ~t =
    bgroup "WordFrequencies"
        [ bench "Text"       $ whnf (frequencies . T.words . T.toLower)     t
        ]

frequencies :: Ord a => [a] -> Map a Int
frequencies = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty
