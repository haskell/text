-- | This module contains a number of benchmarks for the different streaming
-- functions
--
-- Tested in this benchmark:
--
-- * Most streaming functions
--
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Text.Benchmarks.Stream
    ( benchmark
    ) where

import Control.DeepSeq (NFData (..))
import Criterion (Benchmark, bgroup, bench, nf)
import Data.Text.Fusion.Internal (Step (..), Stream (..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding.Error as E
import qualified Data.Text.Encoding.Fusion as T
import qualified Data.Text.Lazy.Encoding.Fusion as TL

instance NFData a => NFData (Stream a) where
    -- Currently, this implementation does not force evaluation of the size hint
    rnf (Stream next s0 _) = go s0
      where
        go !s = case next s of
            Done       -> ()
            Skip s'    -> go s'
            Yield x s' -> rnf x `seq` go s'

benchmark :: FilePath -> IO Benchmark
benchmark fp = do
    -- Load data
    bs  <- B.readFile fp
    lbs <- BL.readFile fp

    return $ bgroup "Stream"
        [ bgroup "streamUtf8"
            [ bench "Text"     $ nf (T.streamUtf8 E.lenientDecode) bs
            , bench "LazyText" $ nf (TL.streamUtf8 E.lenientDecode) lbs
            ]
        ]
