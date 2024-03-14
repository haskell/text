-- | Benchmarks on artificial data. 

module Benchmarks.Micro (benchmark) where

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Test.Tasty.Bench (Benchmark, bgroup, bench, nf)

benchmark :: Benchmark
benchmark = bgroup "Micro"
  [ -- Accessing i-th element should take O(i) time.
    -- The 2k case should run in 2x the time of the 1k case.
    bgroup "Lazy.inits"
      [ bench "last 1k" $ nf (last . TL.inits) (chunks 1000)
      , bench "last 2k" $ nf (last . TL.inits) (chunks 2000)
      , bench "map-take1 1k" $ nf (map (TL.take 1) . TL.inits) (chunks 1000)
      , bench "map-take1 2k" $ nf (map (TL.take 1) . TL.inits) (chunks 2000)
      ]
  ]

chunks :: Int -> TL.Text
chunks n = TL.fromChunks (replicate n (T.pack "a"))
