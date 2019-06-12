-- | Compare a string after a transformation
--
-- Tested in this benchmark:
--
-- * Comparison of transformed strings via transformEq
--
module Benchmarks.TransformEq
    ( benchmark
    ) where

import Criterion (Benchmark, bgroup, bench, whnf)
import Data.Function (on)
import Data.Text (transformEq, toCaseFold, pack)

benchmark :: IO Benchmark
benchmark = do
    let
      equiv = (pack "Fooooooo", pack "fOOOOOOO")
      notEq = (pack "fooooooo", pack "barrrrrr")
      lengthNotEq = (pack "foo", pack "foooooooo")
      eq = uncurry ((==) `on` toCaseFold)
      transEq = uncurry $ transformEq toCaseFold
    return $ bgroup "transformEq"
        [ bench "Text ==: Eq" $ whnf eq equiv
        , bench "Text transformEq: Eq" $ whnf transEq equiv
        , bench "Text ==: Not Eq" $ whnf eq notEq
        , bench "Text transformEq: Not Eq" $ whnf transEq notEq
        , bench "Text ==: Not Length" $ whnf eq lengthNotEq
        , bench "Text transformEq: Not Length" $ whnf transEq lengthNotEq
        ]
