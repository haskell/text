-- | UTF-8 encode a text
--
-- Tested in this benchmark:
--
-- * Replicating a string a number of times
--
-- * UTF-8 encoding it
--
module Benchmarks.EncodeUtf8
    ( benchmark
    ) where

import Test.Tasty.Bench (Benchmark, bgroup, bench, nf, whnf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

benchmark :: String -> String -> Benchmark
benchmark name string =
    bgroup name
        [ bench "Text"     $ whnf (B.length . T.encodeUtf8)   text
        , bench "LazyText" $ whnf (BL.length . TL.encodeUtf8) lazyText
        , bench "Text/encodeUtf8Builder" $ nf (B.toLazyByteString . T.encodeUtf8Builder) text
        , bench "Text/encodeUtf8BuilderEscaped" $ nf (B.toLazyByteString . T.encodeUtf8BuilderEscaped (BP.liftFixedToBounded BP.word8)) text
        ]
  where
    -- The string in different formats
    text = T.replicate k $ T.pack string
    lazyText = TL.replicate (fromIntegral k) $ TL.pack string

    -- Amount
    k = 100000
