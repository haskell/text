-- | Read numbers from a file with a just a number on each line, find the
-- minimum of those numbers.
--
module Data.Text.Benchmarks.ReadNumbers
    ( benchmark
    ) where

import Control.Exception (evaluate)
import Criterion (Benchmark, bgroup, bench)
import Data.List (foldl')
import Numeric (readDec, readFloat, readHex)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lex.Double as B
import qualified Data.ByteString.Lex.Lazy.Double as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Read as TL
import qualified Data.Text.Read as T

benchmark :: FilePath -> IO Benchmark
benchmark fp = return $ bgroup "ReadNumbers"
    [ bench "DecimalString" $ readFile fp >>= evaluate .
        int . string readDec . lines
    , bench "HexadecimalString" $ readFile fp >>= evaluate .
        int . string readHex . lines
    , bench "DoubleString" $ readFile fp >>= evaluate .
        double . string readFloat . lines

    , bench "DecimalText" $ B.readFile fp >>= evaluate .
        int . text (T.signed T.decimal) . T.lines . T.decodeUtf8
    , bench "HexadecimalText" $ B.readFile fp >>= evaluate .
        int . text (T.signed T.hexadecimal) . T.lines . T.decodeUtf8
    , bench "DoubleText" $ B.readFile fp >>= evaluate .
        double . text T.double . T.lines . T.decodeUtf8
    , bench "RationalText" $ B.readFile fp >>= evaluate .
        double . text T.rational . T.lines . T.decodeUtf8

    , bench "DecimalLazyText" $ BL.readFile fp >>= evaluate .
        int . text (TL.signed TL.decimal) . TL.lines . TL.decodeUtf8
    , bench "HexadecimalLazyText" $ BL.readFile fp >>= evaluate .
        int . text (TL.signed TL.hexadecimal) . TL.lines . TL.decodeUtf8
    , bench "DoubleLazyText" $ BL.readFile fp >>= evaluate .
        double . text TL.double . TL.lines . TL.decodeUtf8
    , bench "RationalLazyText" $ BL.readFile fp >>= evaluate .
        double . text TL.rational . TL.lines . TL.decodeUtf8

    , bench "DecimalByteString" $ B.readFile fp >>= evaluate .
        int . byteString B.readInt . B.lines
    , bench "DoubleByteString" $ B.readFile fp >>= evaluate .
        double . byteString B.readDouble . B.lines

    , bench "DecimalLazyByteString" $ BL.readFile fp >>= evaluate .
        int . byteString BL.readInt . BL.lines
    , bench "DoubleLazyByteString" $ BL.readFile fp >>= evaluate .
        double . byteString BL.readDouble . BL.lines
    ]
  where
    -- Used for fixing types
    int :: Int -> Int
    int = id
    double :: Double -> Double
    double = id

string :: (Ord a, Num a) => (t -> [(a, t)]) -> [t] -> a
string reader = foldl' go 1000000
  where
    go z t = case reader t of [(n, _)] -> min n z
                              _        -> z

text :: (Ord a, Num a) => (t -> Either String (a,t)) -> [t] -> a
text reader = foldl' go 1000000
  where
    go z t = case reader t of Left _       -> z
                              Right (n, _) -> min n z
    
byteString :: (Ord a, Num a) => (t -> Maybe (a,t)) -> [t] -> a
byteString reader = foldl' go 1000000
  where
    go z t = case reader t of Nothing     -> z
                              Just (n, _) -> min n z
