-- | Implements the unix @sort@ program
--
{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Benchmarks.Sort
    ( benchmark
    ) where

import Criterion (Benchmark, bench)
import Data.Monoid (mconcat)
import System.IO (Handle)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TL

benchmark :: FilePath -> Handle -> IO Benchmark
benchmark fp sink = return $ bench "Sort" $ do
    t <- T.decodeUtf8 `fmap` B.readFile fp
    BL.hPutStr sink $ TL.encodeUtf8 $ sort t

sort :: T.Text -> TL.Text
sort = TLB.toLazyText . mconcat . L.intersperse (TLB.fromText "\n") .
    map TLB.fromText . L.sort . T.lines
