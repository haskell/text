-- | Search for a pattern in a file, find the number of occurences
--
module Data.Text.Benchmarks.FileIndices
    ( benchmark
    ) where

import Control.Exception (evaluate)
import Criterion (Benchmark, bench, bgroup)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Search as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL

benchmark :: FilePath -> TL.Text -> IO Benchmark
benchmark fp t = return $ bgroup "FileIndices"
    [ bench "LazyText"           $ TL.readFile fp >>= evaluate . text t
    , bench "LazyByteString"     $ BL.readFile fp >>= evaluate . byteString b
    ]
  where
    b = B.concat $ BL.toChunks $ TL.encodeUtf8 t

text :: TL.Text -> TL.Text -> Int
text needle = fromIntegral . TL.count needle

byteString :: B.ByteString -> BL.ByteString -> Int
byteString needle = length . BL.indices needle
