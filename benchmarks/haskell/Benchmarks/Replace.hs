{-# LANGUAGE BangPatterns #-}
-- | Replace a string by another string
--
-- Tested in this benchmark:
--
-- * Search and replace of a pattern in a text
--
module Benchmarks.Replace
    ( benchmark
    , initEnv
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, nf)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

type Env = (T.Text, TL.Text)

initEnv :: FilePath -> IO Env
initEnv fp = do
    tl <- TL.readFile fp
    let !t = TL.toStrict tl
    return (t, tl)

benchmark :: String -> String -> Env -> Benchmark
benchmark pat sub ~(t, tl) =
    bgroup "Replace" [
          bench "Text"           $ nf (T.length . T.replace tpat tsub) t
        , bench "LazyText"       $ nf (TL.length . TL.replace tlpat tlsub) tl
        ]
  where
    tpat  = T.pack pat
    tsub  = T.pack sub
    tlpat = TL.pack pat
    tlsub = TL.pack sub
