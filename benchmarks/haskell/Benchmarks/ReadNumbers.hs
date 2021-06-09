-- | Read numbers from a file with a just a number on each line, find the
-- minimum of those numbers. The file contains different kinds of numbers:
--
-- * Decimals
--
-- * Hexadecimals
--
-- * Floating point numbers
--
-- * Floating point numbers in scientific notation
--
-- The different benchmarks will only take into account the values they can
-- parse.
--
-- Tested in this benchmark:
--
-- * Lexing/parsing of different numerical types
--
module Benchmarks.ReadNumbers
    ( initEnv
    , benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, whnf)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Read as TL
import qualified Data.Text.Read as T

type Env = ([T.Text], [TL.Text])

initEnv :: FilePath -> IO Env
initEnv fp = do
    t <- T.lines `fmap` T.readFile fp
    tl <- TL.lines `fmap` TL.readFile fp
    return (t, tl)

benchmark :: Env -> Benchmark
benchmark ~(t, tl) =
    bgroup "ReadNumbers"
        [ bench "DecimalText"     $ whnf (int . text (T.signed T.decimal)) t
        , bench "HexadecimalText" $ whnf (int . text (T.signed T.hexadecimal)) t
        , bench "DoubleText"      $ whnf (double . text T.double) t
        , bench "RationalText"    $ whnf (double . text T.rational) t

        , bench "DecimalLazyText" $
            whnf (int . text (TL.signed TL.decimal)) tl
        , bench "HexadecimalLazyText" $
            whnf (int . text (TL.signed TL.hexadecimal)) tl
        , bench "DoubleLazyText" $
            whnf (double . text TL.double) tl
        , bench "RationalLazyText" $
            whnf (double . text TL.rational) tl
        ]
  where
    -- Used for fixing types
    int :: Int -> Int
    int = id
    double :: Double -> Double
    double = id

text :: (Ord a, Num a) => (t -> Either String (a,t)) -> [t] -> a
text reader = foldl' go 1000000
  where
    go z t = case reader t of Left _       -> z
                              Right (n, _) -> min n z
