-- | Benchmarks on artificial data. 

module Benchmarks.Micro (benchmark) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Test.Tasty.Bench (Benchmark, Benchmarkable, bgroup, bcompareWithin, bench, nf)

benchmark :: Benchmark
benchmark = bgroup "Micro"
  [ blinear "lazy-inits--last" 500000 2 0.1 $ \len ->
      nf (NE.last . TL.initsNE) (chunks len)
  , blinear "lazy-inits--map-take1" 500000 2 0.1 $ \len ->
      nf (map (TL.take 1) . TL.inits) (chunks len)
  ]

chunks :: Int -> TL.Text
chunks n = TL.fromChunks (replicate n (T.pack "a"))

-- Check that running an action with input length (m * baseLen)
-- runs m times slower than the same action with input length baseLen.
blinear :: String  -- ^ Name (must be globally unique!)
        -> Int     -- ^ Base length
        -> Int     -- ^ Multiplier m
        -> Double  -- ^ Slack s
        -> (Int -> Benchmarkable)  -- ^ Action to measure, parameterized by input length
        -> Benchmark
blinear name baseLen m s run = bgroup name
  [ bench "baseline" $ run baseLen
  , bcompareWithin (fromIntegral m * (1 - s)) (fromIntegral m * (1 + s)) (name ++ ".baseline") $
      bench ("x" ++ show m) $ run (m * baseLen)
  ]
