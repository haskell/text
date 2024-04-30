-- | Benchmarks simple file writing
--
-- Tested in this benchmark:
--
-- * Writing a file to the disk
--

{-# LANGUAGE BangPatterns #-}

module Benchmarks.FileWrite
    ( mkFileWriteBenchmarks
    ) where

import Control.DeepSeq (NFData, deepseq)
import Data.Bifunctor (first)
import Data.Semigroup ((<>))
import Data.String (fromString)
import Data.Text (StrictText)
import Data.Text.Lazy (LazyText)
import System.IO (Handle, Newline(CRLF,LF), NewlineMode(NewlineMode), BufferMode(NoBuffering,LineBuffering,BlockBuffering), hSetBuffering, hSetNewlineMode)
import Test.Tasty.Bench (Benchmark, bgroup, bench, nfAppIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.IO.Utf8 as Utf8
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

mkFileWriteBenchmarks :: IO (Handle, IO ()) -> IO (Benchmark, IO ())
mkFileWriteBenchmarks mkSinkNRemove = do
  let writeDate = LT.cycle $ fromString [minBound..maxBound]
      lengths = [0..5] <> [10,20..100] <> [1000,10000,100000]
      testGroup :: NFData text => String -> (Handle -> text -> IO ()) -> ((StrictText,LazyText) -> text) -> Newline -> IO (Benchmark, IO ())
      testGroup groupName hPutStr select nl = do
        let nlm = NewlineMode nl nl
        (!noBufH, noBufRm) <- mkSinkNRemove
        hSetBuffering noBufH NoBuffering
        hSetNewlineMode noBufH nlm
        (!lineBufH, lineBufRm) <- mkSinkNRemove
        hSetBuffering lineBufH LineBuffering
        hSetNewlineMode lineBufH nlm
        (!blockBufH, blockBufRm) <- mkSinkNRemove
        hSetBuffering blockBufH $ BlockBuffering Nothing
        hSetNewlineMode blockBufH nlm

        pure
          ( bgroup (groupName <> " " <> show nl) $ lengths <&> \n -> let
              st = LT.toStrict lt
              lt = LT.take n writeDate
              t = select (st, lt)
              in bgroup ("length " <> show n) $ deepseq t
                [ bench "NoBuffering"    $ nfAppIO (hPutStr noBufH)    t
                , bench "LineBuffering"  $ nfAppIO (hPutStr lineBufH)  t
                , bench "BlockBuffering" $ nfAppIO (hPutStr blockBufH) t
                ]
          , do
              noBufRm
              lineBufRm
              blockBufRm
          )
  first (bgroup "FileWrite")
    . foldr (\(b,r) (bs,rs) -> (b:bs,r>>rs)) ([], return ())
    <$> sequence
    [ testGroup "Strict hPutStr" T.hPutStr    strict LF
    , testGroup "Lazy   hPutStr large chunks" LT.hPutStr   lazyLargeChunks   LF
    , testGroup "Lazy   hPutStr small chunks" LT.hPutStr   lazySmallChunks   LF
    , testGroup "Strict hPutStr" T.hPutStr    strict CRLF
    , testGroup "Lazy   hPutStr large chunks" LT.hPutStr   lazyLargeChunks   CRLF
    , testGroup "Lazy   hPutStr small chunks" LT.hPutStr   lazySmallChunks   CRLF
    , testGroup "Utf-8  hPutStr" Utf8.hPutStr strict LF
    ]

  where
  strict = fst
  lazyLargeChunks = snd
  lazySmallChunks = LT.fromChunks . T.chunksOf 10 . fst

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
