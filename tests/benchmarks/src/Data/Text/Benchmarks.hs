-- | Main module to run the micro benchmarks
--
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Criterion.Main (Benchmark, defaultMain, bgroup)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), openFile, hSetEncoding, utf8)

import qualified Data.Text.Benchmarks.Builder as Builder
import qualified Data.Text.Benchmarks.DecodeUtf8 as DecodeUtf8
import qualified Data.Text.Benchmarks.EncodeUtf8 as EncodeUtf8
import qualified Data.Text.Benchmarks.Equality as Equality
import qualified Data.Text.Benchmarks.FileIndices as FileIndices
import qualified Data.Text.Benchmarks.FileRead as FileRead
import qualified Data.Text.Benchmarks.FoldLines as FoldLines
import qualified Data.Text.Benchmarks.HtmlCombinator as HtmlCombinator
import qualified Data.Text.Benchmarks.Ordering as Ordering
import qualified Data.Text.Benchmarks.Pure as Pure
import qualified Data.Text.Benchmarks.ReadNumbers as ReadNumbers
import qualified Data.Text.Benchmarks.Replace as Replace
import qualified Data.Text.Benchmarks.Sort as Sort
import qualified Data.Text.Benchmarks.StripBrackets as StripBrackets
import qualified Data.Text.Benchmarks.WordCount as WordCount

import qualified Data.Text.Benchmarks.Programs.Cut as Programs.Cut

main :: IO ()
main = benchmarks >>= defaultMain

benchmarks :: IO [Benchmark]
benchmarks = do
    sink <- openFile "/dev/null" WriteMode
    hSetEncoding sink utf8

    -- Traditional benchmarks
    bs <- sequence
        [ Builder.benchmark
        , DecodeUtf8.benchmark (tf "russian.txt")
        , EncodeUtf8.benchmark sink "επανάληψη 竺法蘭共譯"
        , Equality.benchmark (tf "japanese.txt")
        , FileIndices.benchmark (tf "russian.txt") "принимая"
        , FileRead.benchmark (tf "russian.txt")
        , FoldLines.benchmark (tf "russian.txt")
        , HtmlCombinator.benchmark sink
        , Ordering.benchmark (tf "russian.txt")
        , Pure.benchmark (tf "japanese.txt")
        , ReadNumbers.benchmark (tf "numbers.txt")
        , Replace.benchmark (tf "russian.txt") sink "принимая" "своем"
        , Sort.benchmark (tf "russian.txt") sink
        , StripBrackets.benchmark (tf "russian.txt") sink
        , WordCount.benchmark (tf "russian.txt")
        ]

    -- Program-like benchmarks
    ps <- bgroup "Programs" `fmap` sequence
        [ Programs.Cut.benchmark (tf "russian.txt") sink 20 40
        ]

    return $ bs ++ [ps]
  where
    -- Location of a test file
    tf = ("../text/test" </>)
