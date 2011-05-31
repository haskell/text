module Data.Text.Benchmarks.EncodeUtf8
    ( benchmark
    ) where

import Criterion (Benchmark, bgroup, bench)
import System.IO (Handle, hPutStr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL

benchmark :: Handle -> String -> IO Benchmark
benchmark sink string = return $ bgroup "EncodeUtf8"
    [ bench "String" $ hPutStr sink $ concat $ replicate k string
    , bench "Text" $ T.hPutStr sink $ T.replicate k text
    , bench "TextLazy" $ TL.hPutStr sink $
        TL.replicate (fromIntegral k) lazyText
    , bench "TextByteString" $ B.hPutStr sink $
        T.encodeUtf8 $ T.replicate k text
    , bench "TextByteStringLazy" $ BL.hPutStr sink $
        TL.encodeUtf8 $ TL.replicate (fromIntegral k) lazyText
    ]
  where
    -- The string in different formats
    text = T.pack string
    lazyText = TL.pack string

    -- Amount
    k = 100000
