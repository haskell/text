-- | Benchmarks simple file writing
--
-- Tested in this benchmark:
--
-- * Writing a file to the disk
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Benchmarks.FileWrite
    ( mkFileWriteBenchmarks
    ) where

import Control.DeepSeq (NFData, deepseq)
import Data.Bifunctor (first)
import Data.List (intercalate, intersperse)
import Data.String (fromString)
import Data.Text (StrictText)
import Data.Text.Internal.Lazy (LazyText, defaultChunkSize)
import System.IO (Handle, Newline(CRLF,LF), NewlineMode(NewlineMode), BufferMode(..), hSetBuffering, hSetNewlineMode)
import Test.Tasty.Bench (Benchmark, bgroup, bench, whnfAppIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.IO.Utf8 as Utf8
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

mkFileWriteBenchmarks :: IO (Handle, IO ()) -> IO (Benchmark, IO ())
mkFileWriteBenchmarks mkSinkNRemove = do
  let writeData = L.cycle $ fromString [minBound..maxBound]

#ifdef ExtendedBenchmarks
      lengths = [0..5] <> [10,20..100] <> [1000,3000,10000,100000]
#else
      lengths = [0,1,100,3000,10000,100000]
#endif

      testGroup :: NFData text => (Handle -> text -> IO ()) -> ((String, StrictText -> text)) -> Newline -> BufferMode -> IO (Benchmark, IO ())
      testGroup hPutStr (textCharacteristics, select) nl mode = do
        (h, removeFile) <- mkSinkNRemove
        hSetBuffering h mode
        hSetNewlineMode h $ NewlineMode nl nl
        pure
          ( bgroup (intercalate " " [textCharacteristics, show nl, show mode]) $
            lengths <&> \n -> let
              t = select $ L.toStrict $ L.take n writeData
              in bench ("length " <> show n)
                $ deepseq t
                $ whnfAppIO (hPutStr h) t
          , removeFile
          )

  sequenceGroup "FileWrite hPutStr"
#ifdef ExtendedBenchmarks
    [ testGroup T.hPutStr strict                  LF   NoBuffering
    , testGroup L.hPutStr lazy                    LF   NoBuffering

    , testGroup T.hPutStr strict                  LF   LineBuffering
    , testGroup T.hPutStr strict                  CRLF LineBuffering
    , testGroup T.hPutStr strictNewlines          LF   LineBuffering
    , testGroup T.hPutStr strictNewlines          CRLF LineBuffering

    , testGroup L.hPutStr lazy                    LF   LineBuffering
    , testGroup L.hPutStr lazy                    CRLF LineBuffering
    , testGroup L.hPutStr lazySmallChunks         LF   LineBuffering
    , testGroup L.hPutStr lazySmallChunks         CRLF LineBuffering
    , testGroup L.hPutStr lazyNewlines            LF   LineBuffering
    , testGroup L.hPutStr lazyNewlines            CRLF LineBuffering
    , testGroup L.hPutStr lazySmallChunksNewlines LF   LineBuffering
    , testGroup L.hPutStr lazySmallChunksNewlines CRLF LineBuffering

    , testGroup T.hPutStr strict                  LF   (BlockBuffering Nothing)
    , testGroup T.hPutStr strict                  CRLF (BlockBuffering Nothing)
    , testGroup T.hPutStr strictNewlines          LF   (BlockBuffering Nothing)
    , testGroup T.hPutStr strictNewlines          CRLF (BlockBuffering Nothing)

    , testGroup L.hPutStr lazy                    LF   (BlockBuffering Nothing)
    , testGroup L.hPutStr lazy                    CRLF (BlockBuffering Nothing)
    , testGroup L.hPutStr lazySmallChunks         LF   (BlockBuffering Nothing)
    , testGroup L.hPutStr lazySmallChunks         CRLF (BlockBuffering Nothing)
    , testGroup L.hPutStr lazyNewlines            LF   (BlockBuffering Nothing)
    , testGroup L.hPutStr lazyNewlines            CRLF (BlockBuffering Nothing)
    , testGroup L.hPutStr lazySmallChunksNewlines LF   (BlockBuffering Nothing)
    , testGroup L.hPutStr lazySmallChunksNewlines CRLF (BlockBuffering Nothing)

    , sequenceGroup "UTF-8"
      [ testGroup Utf8.hPutStr strict LF NoBuffering
      , testGroup Utf8.hPutStr strict LF LineBuffering
      , testGroup Utf8.hPutStr strict LF (BlockBuffering Nothing)
      ]
    ]
#else
    [ testGroup T.hPutStr strictNewlines LF LineBuffering
    , testGroup T.hPutStr strictNewlines CRLF LineBuffering

    , testGroup T.hPutStr strict LF (BlockBuffering Nothing)
    , testGroup T.hPutStr strictNewlines CRLF (BlockBuffering Nothing)

    , testGroup L.hPutStr lazyNewlines LF LineBuffering
    , testGroup L.hPutStr lazyNewlines CRLF LineBuffering

    , testGroup L.hPutStr lazy LF (BlockBuffering Nothing)
    , testGroup L.hPutStr lazyNewlines CRLF (BlockBuffering Nothing)

    , sequenceGroup "UTF-8"
      [ testGroup Utf8.hPutStr strict LF LineBuffering
      , testGroup Utf8.hPutStr strict LF (BlockBuffering Nothing)
      ]
    ]
#endif

  where
  lazy, lazyNewlines :: (String, StrictText -> LazyText)
  lazy                    = ("lazy",                            L.fromChunks . T.chunksOf defaultChunkSize)
  lazyNewlines            = ("lazy many newlines",              snd lazy . snd strictNewlines)

#ifdef ExtendedBenchmarks
  lazySmallChunks, lazySmallChunksNewlines :: (String, StrictText -> LazyText)
  lazySmallChunks         = ("lazy small chunks",               L.fromChunks . T.chunksOf 10)
  lazySmallChunksNewlines = ("lazy small chunks many newlines", snd lazySmallChunks . snd strictNewlines)
#endif

  strict, strictNewlines :: (String, StrictText -> StrictText)
  strict                  = ("strict",                          id)
  strictNewlines          = ("strict many newlines",            mconcat . intersperse "\n" . T.chunksOf 5)

  sequenceGroup groupName tgs
    =   first (bgroup groupName)
    .   foldr (\(b,r) (bs,rs) -> (b:bs,r>>rs)) ([], return ())
    <$> sequence tgs

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

