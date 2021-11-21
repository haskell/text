{-# LANGUAGE ForeignFunctionInterface #-}

-- | Test decoding of UTF-8
--
-- Tested in this benchmark:
--
-- * Decoding bytes using UTF-8
--
-- In some tests:
--
-- * Taking the length of the result
--
-- * Taking the init of the result
--
-- The latter are used for testing stream fusion.
--
module Benchmarks.DecodeUtf8
    ( initEnv
    , benchmark
    , benchmarkASCII
    ) where

import Data.ByteString.Lazy.Internal (ByteString(..))
import Test.Tasty.Bench
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

type Env = (B.ByteString, BL.ByteString)

initEnv :: FilePath -> IO Env
initEnv fp = do
    bs  <- B.readFile fp
    lbs <- BL.readFile fp
    return (bs, lbs)

benchmark :: String -> Env -> Benchmark
benchmark kind ~(bs, lbs) =
    let decodeStream (Chunk b0 bs0) = case T.streamDecodeUtf8 b0 of
                                        T.Some t0 _ f0 -> t0 : go f0 bs0
          where go f (Chunk b bs1) = case f b of
                                       T.Some t1 _ f1 -> t1 : go f1 bs1
                go _ _ = []
        decodeStream _ = []
    in bgroup kind
        [ bench "Strict" $ nf T.decodeUtf8 bs
        , bench "Stream" $ nf decodeStream lbs
        , bench "StrictLength" $ nf (T.length . T.decodeUtf8) bs
        , bench "StrictInitLength" $ nf (T.length . T.init . T.decodeUtf8) bs
        , bench "Lazy" $ nf TL.decodeUtf8 lbs
        , bench "LazyLength" $ nf (TL.length . TL.decodeUtf8) lbs
        , bench "LazyInitLength" $ nf (TL.length . TL.init . TL.decodeUtf8) lbs
        ]

benchmarkASCII :: Env -> Benchmark
benchmarkASCII ~(bs, lbs) =
    bgroup "ascii"
        [ bench "strict decodeUtf8" $ nf T.decodeUtf8 bs
        , bench "strict decodeLatin1" $ nf T.decodeLatin1 bs
        , bench "strict decodeASCII" $ nf T.decodeASCII bs
        , bench "lazy decodeUtf8" $ nf TL.decodeUtf8 lbs
        , bench "lazy decodeLatin1" $ nf TL.decodeLatin1 lbs
        , bench "lazy decodeASCII" $ nf TL.decodeASCII lbs
        ]
