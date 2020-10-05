-- | ASCII encode a text
--
-- Tested in this benchmark:
--
-- * Replicating a string a number of times
--
-- * ASCII encoding it
--
module Benchmarks.EncodeASCII
    ( benchmark
    ) where

import Criterion (Benchmark, bgroup, bench, whnf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

benchmark :: String -> Benchmark
benchmark string =
    bgroup "EncodeASCII"
        [ bench "Text"     $ whnf (B.length . T.encodeASCII)   text
        , bench "LazyText" $ whnf (BL.length . TL.encodeASCII) lazyText
        ]
  where
    -- The string in different formats
    text = T.replicate k $ T.pack string
    lazyText = TL.replicate (fromIntegral k) $ TL.pack string

    -- Amount
    k = 100000
