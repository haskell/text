-- | Search for a pattern in a file, find the number of occurrences
--
-- Tested in this benchmark:
--
-- * Searching all occurrences of a pattern using library routines
--
module Benchmarks.Search
    ( initEnv
    , benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

type Env = (T.Text, TL.Text)

initEnv :: FilePath -> IO Env
initEnv fp = do
    t  <- T.readFile fp
    tl <- TL.readFile fp
    return (t, tl)

benchmark :: T.Text -> Env -> Benchmark
benchmark needleT ~(t, tl) =
    bgroup "FileIndices"
        [ bench "Text"           $ whnf (text needleT)           t
        , bench "LazyText"       $ whnf (lazyText needleTL)      tl
        ]
  where
    needleTL = TL.fromChunks [needleT]

text :: T.Text -> T.Text -> Int
text = T.count

lazyText :: TL.Text -> TL.Text -> Int
lazyText needle = fromIntegral . TL.count needle
