-- | Benchmark which formats paragraph, like the @sort@ unix utility.
--
-- Tested in this benchmark:
--
-- * Reading the file
--
-- * Splitting into paragraphs
--
-- * Reformatting the paragraphs to a certain line width
--
-- * Concatenating the results using the text builder
--
-- * Writing back to a handle
--
{-# LANGUAGE CPP, OverloadedStrings #-}
module Benchmarks.Programs.Fold
    ( benchmark
    ) where

import Data.Foldable (Foldable(..))
import Data.List (intersperse)
import Prelude hiding (Foldable(..))
import System.IO (Handle)
import Test.Tasty.Bench (Benchmark, bench, whnfIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

benchmark :: FilePath -> Handle -> Benchmark
benchmark i o =
    bench "Fold" $ whnfIO $ T.readFile i >>= TL.hPutStr o . foldText 80

-- | We represent a paragraph by a word list
--
type Paragraph = [T.Text]

-- | Fold a text
--
foldText :: Int -> T.Text -> TL.Text
foldText maxWidth = TLB.toLazyText . mconcat .
    intersperse "\n\n" . map (foldParagraph maxWidth) . paragraphs

-- | Fold a paragraph
--
foldParagraph :: Int -> Paragraph -> TLB.Builder
foldParagraph _    []       = mempty
foldParagraph max' (w : ws) = fst $ foldl' go (TLB.fromText w, T.length w) ws
  where
    go (builder, width) word
        | width + len + 1 <= max' =
            (builder `mappend` " " `mappend` word', width + len + 1)
        | otherwise =
            (builder `mappend` "\n" `mappend` word', len)
      where
        word' = TLB.fromText word
        len = T.length word

-- | Divide a text into paragraphs
--
paragraphs :: T.Text -> [Paragraph]
paragraphs = splitParagraphs . map T.words . T.lines
  where
    splitParagraphs ls = case break null ls of
        ([], []) -> []
        (p,  []) -> [concat p]
        (p,  lr) -> concat p : splitParagraphs (dropWhile null lr)
