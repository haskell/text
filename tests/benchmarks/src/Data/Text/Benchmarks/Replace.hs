-- | Replace a string by another string in a file
--
module Data.Text.Benchmarks.Replace
    ( benchmark
    ) where

import Criterion (Benchmark, bgroup, bench)
import System.IO (Handle)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Search as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL

benchmark :: FilePath -> Handle -> String -> String -> IO Benchmark
benchmark fp sink pat sub = return $ bgroup "Replace"
    -- We have benchmarks for lazy text and lazy bytestrings. We also benchmark
    -- without the acual replacement, so we can get an idea of what time is
    -- spent on IO and computations.
    [ bench "LazyText" $ TL.readFile fp >>=
        TL.hPutStr sink . TL.replace tpat tsub
    , bench "LazyTextNull" $ TL.readFile fp >>= TL.hPutStr sink

    , bench "LazyByteString" $ BL.readFile fp >>=
        BL.hPutStr sink . BL.replace bpat bsub
    , bench "LazyByteStringNull" $ BL.readFile fp >>= BL.hPutStr sink
    ]
  where
    tpat = TL.pack pat
    tsub = TL.pack sub
    bpat = B.concat $ BL.toChunks $ TL.encodeUtf8 tpat
    bsub = B.concat $ BL.toChunks $ TL.encodeUtf8 tsub
