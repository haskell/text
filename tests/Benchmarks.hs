{-# LANGUAGE GADTs, MagicHash #-}

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Internal as BL
import Control.Exception (evaluate)
import Control.DeepSeq (NFData(..))
import Control.Parallel.Strategies
import Criterion.Main
import Data.Char
import qualified Codec.Binary.UTF8.Generic as UTF8
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.List as L
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Lazy.Encoding as TL
import qualified Criterion.MultiMap as M
import Criterion.Config
import GHC.Base


myConfig
    | False     = defaultConfig {
                    -- Always display an 800x600 window.
                    cfgPlot = M.singleton KernelDensity (Window 800 600)
                  }
    | otherwise = defaultConfig

instance NFData BS.ByteString

instance NFData BL.ByteString where
    rnf BL.Empty        = ()
    rnf (BL.Chunk _ ts) = rnf ts

data B where
    B :: NFData a => a -> B

instance NFData B where
    rnf (B b) = rnf b

main = do
  bsa <- BS.readFile "text/test/russian.txt"
  let tsa     = TS.decodeUtf8 bsa
      tsb     = TS.toUpper tsa
      tla     = TL.fromChunks (TS.chunksOf 16376 tsa)
      tlb     = TL.fromChunks (TS.chunksOf 16376 tsb)
      bsb     = TS.encodeUtf8 tsb
      bla     = BL.fromChunks (chunksOf 16376 bsa)
      blb     = BL.fromChunks (chunksOf 16376 bsb)
      bsa_len = BS.length bsa
      tsa_len = TS.length tsa
      bla_len = BL.length bla
      tla_len = TL.length tla
      la      = UTF8.toString bsa
      la_len  = L.length la
      tsb_len = TS.length tsb
      lb      = TS.unpack tsb
      bsl     = BS.lines bsa
      bll     = BL.lines bla
      tsl     = TS.lines tsa
      tll     = TL.lines tla
      ll      = L.lines la
  evaluate (rnf [B tsa, B tsb, B tla, B tlb, B bsa, B bsb, B bla, B blb,
                 B bsa_len, B tsa_len, B bla_len, B tla_len, B la, B la_len,
                 B tsb_len, B lb, B bsl, B bll, B tsl, B tll, B ll])
  defaultMainWith myConfig [
      bgroup "append" [
        bench "ts" $ nf (TS.append tsb) tsa
      , bench "tl" $ nf (TL.append tlb) tla
      , bench "bs" $ nf (BS.append bsb) bsa
      , bench "bl" $ nf (BL.append blb) bla
      , bench "l" $ nf ((++) lb) la
      ],
      bgroup "concat" [
        bench "ts" $ nf TS.concat tsl
      , bench "tl" $ nf TL.concat tll
      , bench "bs" $ nf BS.concat bsl
      , bench "bl" $ nf BL.concat bll
      , bench "l" $ nf L.concat ll
      ],
      bgroup "concatMap" [
        bench "ts" $ nf (TS.concatMap (TS.replicate 3 . TS.singleton)) tsa
      , bench "tl" $ nf (TL.concatMap (TL.replicate 3 . TL.singleton)) tla
      , bench "bs" $ nf (BS.concatMap (BS.replicate 3)) bsa
      , bench "bl" $ nf (BL.concatMap (BL.replicate 3)) bla
      , bench "l" $ nf (L.concatMap (L.replicate 3 . (:[]))) la
      ],
      bgroup "decode" [
        bench "ts" $ nf TS.decodeUtf8 bsa
      , bench "tl" $ nf TL.decodeUtf8 bla
      , bench "bs" $ nf BS.unpack bsa
      , bench "bl" $ nf BL.unpack bla
      , bench "l" $ nf UTF8.toString bsa
      ],
      bgroup "drop" [
        bench "ts" $ nf (TS.drop (tsa_len `div` 3)) tsa
      , bench "tl" $ nf (TL.drop (tla_len `div` 3)) tla
      , bench "bs" $ nf (BS.drop (bsa_len `div` 3)) bsa
      , bench "bl" $ nf (BL.drop (bla_len `div` 3)) bla
      , bench "l" $ nf (L.drop (la_len `div` 3)) la
      ],
      bgroup "filter" [
        bench "ts" $ nf (TS.filter p0) tsa
      , bench "tl" $ nf (TL.filter p0) tla
      , bench "bs" $ nf (BS.filter p0) bsa
      , bench "bl" $ nf (BL.filter p0) bla
      , bench "l" $ nf (L.filter p0) la
      ],
      bgroup "2filter" [
        bench "ts" $ nf (TS.filter p1 . TS.filter p0) tsa
      , bench "tl" $ nf (TL.filter p1 . TL.filter p0) tla
      , bench "bs" $ nf (BS.filter p1 . BS.filter p0) bsa
      , bench "bl" $ nf (BL.filter p1 . BL.filter p0) bla
      , bench "l" $ nf (L.filter p1 . L.filter p0) la
      ],
      bgroup "foldl'" [
        bench "ts" $ nf (TS.foldl' len 0) tsa
      , bench "tl" $ nf (TL.foldl' len 0) tla
      , bench "bs" $ nf (BS.foldl' len 0) bsa
      , bench "bl" $ nf (BL.foldl' len 0) bla
      , bench "l" $ nf (L.foldl' len 0) la
      ],
      bgroup "foldr" [
        bench "ts" $ nf (L.length . TS.foldr (:) []) tsa
      , bench "tl" $ nf (L.length . TL.foldr (:) []) tla
      , bench "bs" $ nf (L.length . BS.foldr (:) []) bsa
      , bench "bl" $ nf (L.length . BL.foldr (:) []) bla
      , bench "l" $ nf (L.length . L.foldr (:) []) la
      ],
      bgroup "intercalate" [
        bench "ts" $ nf (TS.intercalate tsw) tsl
      , bench "tl" $ nf (TL.intercalate tlw) tll
      , bench "bs" $ nf (BS.intercalate bsw) bsl
      , bench "bl" $ nf (BL.intercalate blw) bll
      , bench "l" $ nf (L.intercalate lw) ll
      ],
      bgroup "isInfixOf" [
        bench "ts" $ nf (TS.isInfixOf tsw) tsa
      , bench "tl" $ nf (TL.isInfixOf tlw) tla
      , bench "bs" $ nf (BS.isInfixOf bsw) bsa
        -- no isInfixOf for lazy bytestrings
      , bench "l" $ nf (L.isInfixOf lw) la
      ],
      bgroup "last" [
        bench "ts" $ nf TS.last tsa
      , bench "tl" $ nf TL.last tla
      , bench "bs" $ nf BS.last bsa
      , bench "bl" $ nf BL.last bla
      , bench "l" $ nf L.last la
      ],
      bgroup "map" [
        bench "ts" $ nf (TS.map f) tsa
      , bench "tl" $ nf (TL.map f) tla
      , bench "bs" $ nf (BS.map f) bsa
      , bench "bl" $ nf (BL.map f) bla
      , bench "l" $ nf (L.map f) la
      ],
      bgroup "2map" [
        bench "ts" $ nf (TS.map f . TS.map f) tsa
      , bench "tl" $ nf (TL.map f . TL.map f) tla
      , bench "bs" $ nf (BS.map f . BS.map f) bsa
      , bench "bl" $ nf (BL.map f . BL.map f) bla
      , bench "l" $ nf (L.map f . L.map f) la
      ],
      bgroup "reverse" [
        bench "ts" $ nf TS.reverse tsa
      , bench "tl" $ nf TL.reverse tla
      , bench "bs" $ nf BS.reverse bsa
      , bench "bl" $ nf BL.reverse bla
      , bench "l" $ nf L.reverse la
      ],
      bgroup "take" [
        bench "ts" $ nf (TS.take (tsa_len `div` 3)) tsa
      , bench "tl" $ nf (TL.take (tla_len `div` 3)) tla
      , bench "bs" $ nf (BS.take (bsa_len `div` 3)) bsa
      , bench "bl" $ nf (BL.take (bla_len `div` 3)) bla
      , bench "l" $ nf (L.take (la_len `div` 3)) la
      ],
      bgroup "words" [
        bench "ts" $ nf TS.words tsa
      , bench "tl" $ nf TL.words tla
      , bench "bs" $ nf BS.words bsa
      , bench "bl" $ nf BL.words bla
      , bench "l" $ nf L.words la
      ],
      bgroup "zipWith" [
        bench "ts" $ nf (TS.zipWith min tsb) tsa
      , bench "tl" $ nf (TL.zipWith min tlb) tla
      , bench "bs" $ nf (BS.zipWith min bsb) bsa
      , bench "bl" $ nf (BL.zipWith min blb) bla
      , bench "l" $ nf (L.zipWith min lb) la
      ],
      bgroup "length" [
        bgroup "decode" [
          bench "ts" $ nf (TS.length . TS.decodeUtf8) bsa
        , bench "tl" $ nf (TL.length . TL.decodeUtf8) bla
        , bench "bs" $ nf (L.length . BS.unpack) bsa
        , bench "bl" $ nf (L.length . BL.unpack) bla
        , bench "utf8-string" $ nf (L.length . UTF8.toString) bsa
        ],
        bgroup "drop" [
          bench "ts" $ nf (TS.length . TS.drop (tsa_len `div` 3)) tsa
        , bench "tl" $ nf (TL.length . TL.drop (tla_len `div` 3)) tla
        , bench "bs" $ nf (BS.length . BS.drop (bsa_len `div` 3)) bsa
        , bench "bl" $ nf (BL.length . BL.drop (bla_len `div` 3)) bla
        , bench "l" $ nf (L.length . L.drop (la_len `div` 3)) la
        ],
        bgroup "filter" [
          bench "ts" $ nf (TS.length . TS.filter p0) tsa
        , bench "tl" $ nf (TL.length . TL.filter p0) tla
        , bench "bs" $ nf (BS.length . BS.filter p0) bsa
        , bench "bl" $ nf (BL.length . BL.filter p0) bla
        , bench "l" $ nf (L.length . L.filter p0) la
        ],
        bgroup "2filter" [
          bench "ts" $ nf (TS.length . TS.filter p1 . TS.filter p0) tsa
        , bench "tl" $ nf (TL.length . TL.filter p1 . TL.filter p0) tla
        , bench "bs" $ nf (BS.length . BS.filter p1 . BS.filter p0) bsa
        , bench "bl" $ nf (BL.length . BL.filter p1 . BL.filter p0) bla
        , bench "l" $ nf (L.length . L.filter p1 . L.filter p0) la
        ],
        bgroup "map" [
          bench "ts" $ nf (TS.length . TS.map f) tsa
        , bench "tl" $ nf (TL.length . TL.map f) tla
        , bench "bs" $ nf (BS.length . BS.map f) bsa
        , bench "bl" $ nf (BL.length . BL.map f) bla
        , bench "l" $ nf (L.length . L.map f) la
        ],
        bgroup "2map" [
          bench "ts" $ nf (TS.length . TS.map f . TS.map f) tsa
        , bench "tl" $ nf (TL.length . TL.map f . TL.map f) tla
        , bench "bs" $ nf (BS.length . BS.map f . BS.map f) bsa
        , bench "l" $ nf (L.length . L.map f . L.map f) la
        ],
        bgroup "take" [
          bench "ts" $ nf (TS.length . TS.take (tsa_len `div` 3)) tsa
        , bench "tl" $ nf (TL.length . TL.take (tla_len `div` 3)) tla
        , bench "bs" $ nf (BS.length . BS.take (bsa_len `div` 3)) bsa
        , bench "bl" $ nf (BL.length . BL.take (bla_len `div` 3)) bla
        , bench "l" $ nf (L.length . L.take (la_len `div` 3)) la
        ],
        bgroup "words" [
          bench "ts" $ nf (L.length . TS.words) tsa
        , bench "tl" $ nf (L.length . TL.words) tla
        , bench "bs" $ nf (L.length . BS.words) bsa
        , bench "bl" $ nf (L.length . BL.words) bla
        , bench "l" $ nf (L.length . L.words) la
        ],
        bgroup "zipWith" [
          bench "ts" $ nf (TS.length . TS.zipWith min tsb) tsa
        , bench "tl" $ nf (TL.length . TL.zipWith min tlb) tla
        , bench "bs" $ nf (L.length . BS.zipWith min bsb) bsa
        , bench "bl" $ nf (L.length . BL.zipWith min blb) bla
        , bench "l" $ nf (L.length . L.zipWith min lb) la
        ]
      ]
    ]
  where
    p0 = (== 'й')
    p1 = (/= 'д')
    lw  = "право"
    bsw  = UTF8.fromString lw
    blw  = BL.fromChunks [bsw]
    tsw  = TS.pack lw
    tlw  = TL.fromChunks [tsw]
    f (C# c#) = C# (chr# (ord# c# +# 1#))
    len l _ = l + (1::Int)

chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf k = go
  where
    go t = case BS.splitAt k t of
             (a,b) | BS.null a -> []
                   | otherwise -> a : go b
