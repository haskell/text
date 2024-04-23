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

import System.IO
import Data.String (fromString)
import qualified Data.Text.Lazy as LT
import Test.Tasty.Bench (Benchmark, bgroup, bench, nfIO)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import Control.DeepSeq (deepseq)
import Data.Functor ((<&>))

mkFileWriteBenchmarks :: IO (Handle, IO ()) -> IO (Benchmark, IO ())
mkFileWriteBenchmarks mkSinkNRemove = do
  let writeDate = LT.cycle $ fromString [minBound..maxBound]
      lengths = [0..5] <> [10,20..100] <> [1000,10000,100000]
      newlineSelect nl = do
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

        return
          ( bgroup ("Newline " <> show nl) $ lengths <&> \n -> let
              st = LT.toStrict lt
              lt = LT.take n writeDate
              in bgroup ("length " <> show n)
                  [ deepseq st $ bgroup "StrictText"
                    [ bench "NoBuffering" $ nfIO $ T.hPutStr noBufH st
                    , bench "LineBuffering" $ nfIO $ T.hPutStr lineBufH st
                    , bench "BlockBuffering" $ nfIO $ T.hPutStr blockBufH st
                    ]
                  , deepseq lt $ bgroup "LazyText"
                    [ bench "NoBuffering" $ nfIO $ LT.hPutStr noBufH lt
                    , bench "LineBuffering" $ nfIO $ LT.hPutStr lineBufH lt
                    , bench "BlockBuffering" $ nfIO $ LT.hPutStr blockBufH lt
                    ]
                  ]
          , do
              noBufRm
              lineBufRm
              blockBufRm
          )

  (lfB, lfR) <- newlineSelect LF
  (crlfB, crlfR) <- newlineSelect CRLF
  return
    ( bgroup "FileWrite" [lfB, crlfB]
    , lfR >> crlfR
    )

