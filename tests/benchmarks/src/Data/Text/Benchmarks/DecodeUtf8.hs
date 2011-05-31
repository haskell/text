module Data.Text.Benchmarks.DecodeUtf8
    ( benchmark
    ) where

import Control.DeepSeq (rnf)
import Criterion (Benchmark, bgroup, bench)
import System.IO (IOMode (ReadMode), openFile, hGetContents, hSetEncoding, utf8)
import qualified Codec.Binary.UTF8.Generic as U8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL

benchmark :: FilePath -> IO Benchmark
benchmark fp = return $ bgroup "DecodeUtf8"
    [ bench "Strict" $ do
        bs <- B.readFile fp
        rnf (T.decodeUtf8 bs) `seq` return ()

    , bench "StrictLength" $ do
        bs <- B.readFile fp
        rnf (T.length $ T.decodeUtf8 bs) `seq` return ()

    , bench "StrictInitLength" $ do
        bs <- B.readFile fp
        rnf (T.length $ T.init $ T.decodeUtf8 bs) `seq` return ()

    , bench "StrictIO" $ do
        h <- openFile fp ReadMode
        hSetEncoding h utf8
        t <- T.hGetContents h
        rnf t `seq` return ()

    , bench "StrictLengthIO" $ do
        h <- openFile fp ReadMode
        hSetEncoding h utf8
        t <- T.hGetContents h
        rnf (T.length t) `seq` return ()

    , bench "Lazy" $ do
        bs <- BL.readFile fp
        rnf (TL.decodeUtf8 bs) `seq` return ()

    , bench "LazyLength" $ do
        bs <- BL.readFile fp
        rnf (TL.length $ TL.decodeUtf8 bs) `seq` return ()

    , bench "LazyInitLength" $ do
        bs <- BL.readFile fp
        rnf (TL.length $ TL.init $ TL.decodeUtf8 bs) `seq` return ()

    , bench "LazyIO" $ do
        h <- openFile fp ReadMode
        hSetEncoding h utf8
        t <- TL.hGetContents h
        rnf t `seq` return ()

    , bench "LazyLengthIO" $ do
        h <- openFile fp ReadMode
        hSetEncoding h utf8
        t <- TL.hGetContents h
        rnf (TL.length t) `seq` return ()

    , bench "String" $ do
        h <- openFile fp ReadMode
        hSetEncoding h utf8
        t <- hGetContents h
        rnf t `seq` return ()

    , bench "StringLength" $ do
        h <- openFile fp ReadMode
        hSetEncoding h utf8
        t <- hGetContents h
        rnf (length t) `seq` return ()

    , bench "LazyStringUtf8" $ do
        s <- U8.toString `fmap` BL.readFile fp
        rnf s `seq` return ()

    , bench "LazyStringUtf8Length" $ do
        s <- U8.toString `fmap` BL.readFile fp
        rnf (length s) `seq` return ()

    , bench "StrictStringUtf8" $ do
        s <- U8.toString `fmap` B.readFile fp
        rnf s `seq` return ()

    , bench "StrictStringUtf8Length" $ do
        s <- U8.toString `fmap` B.readFile fp
        rnf (length s) `seq` return ()
    ]
