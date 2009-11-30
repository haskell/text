{-# LANGUAGE MagicHash #-}

import qualified Data.ByteString.Char8 as B8
import Control.Exception (evaluate)
import Control.DeepSeq (NFData(..))
import Criterion.Main
import Data.Char
import qualified Codec.Binary.UTF8.Generic as UTF8
import qualified Data.Text as T
import qualified Data.List as L
import Data.Text.Encoding
import qualified Criterion.MultiMap as M
import Criterion.Config
import GHC.Base


myConfig
    | False     = defaultConfig {
                    -- Always display an 800x600 window.
                    cfgPlot = M.singleton KernelDensity (Window 800 600)
                  }
    | otherwise = defaultConfig

instance NFData B8.ByteString

main = do
  s0 <- B8.readFile "text/test/russian.txt"
  let !s0l = B8.length s0
      t0   = decodeUtf8 s0
      !t0l = T.length t0
      l0   = UTF8.toString s0
      !l0l = L.length l0
      s1   = B8.map toUpper s0
      !s1l = B8.length s1
      t1   = T.toUpper t0
      !t1l = T.length t1
      l1   = L.map toUpper l0
      !l1l = L.length l1
      sl   = B8.lines s0
      tl   = T.lines t0
      ll   = L.lines l0
  evaluate (rnf (sl,tl,ll))
  defaultMainWith myConfig [
      bgroup "append" [
        bench "text" $ nf (T.append t1) t0
      , bench "b8" $ nf (B8.append s1) s0
      , bench "list" $ nf ((++) l1) l0
      ],
      bgroup "concat" [
        bench "text" $ nf T.concat tl
      , bench "b8" $ nf B8.concat sl
      , bench "list" $ nf L.concat ll
      ],
      bgroup "decode" [
        bench "text" $ nf decodeUtf8 s0
      , bench "b8" $ nf B8.unpack s0
      , bench "utf8-string" $ nf UTF8.toString s0
      ],
      bgroup "drop" [
        bench "text" $ nf (T.drop (t0l `div` 3)) t0
      , bench "b8" $ nf (B8.drop (s0l `div` 3)) s0
      , bench "list" $ nf (L.drop (l0l `div` 3)) l0
      ],
      bgroup "filter" [
        bench "text" $ nf (T.filter p0) t0
      , bench "b8" $ nf (B8.filter p0) s0
      , bench "list" $ nf (L.filter p0) l0
      ],
      bgroup "2filter" [
        bench "text" $ nf (T.filter p1 . T.filter p0) t0
      , bench "b8" $ nf (B8.filter p1 . B8.filter p0) s0
      , bench "list" $ nf (L.filter p1 . L.filter p0) l0
      ],
      bgroup "foldl'" [
        bench "text" $ nf (T.foldl' len 0) t0
      , bench "b8" $ nf (B8.foldl' len 0) s0
      , bench "list" $ nf (L.foldl' len 0) l0
      ],
      bgroup "foldr" [
        bench "text" $ nf (L.length . T.foldr (:) []) t0
      , bench "b8" $ nf (L.length . B8.foldr (:) []) s0
      , bench "list" $ nf (L.length . L.foldr (:) []) l0
      ],
      bgroup "intercalate" [
        bench "text" $ nf (T.intercalate tw) tl
      , bench "b8" $ nf (B8.intercalate sw) sl
      , bench "list" $ nf (L.intercalate lw) ll
      ],
      bgroup "isInfixOf" [
        bench "text" $ nf (T.isInfixOf tw) t0
      , bench "b8" $ nf (B8.isInfixOf sw) s0
      , bench "list" $ nf (L.isInfixOf lw) l0
      ],
      bgroup "last" [
        bench "text" $ nf T.last t0
      , bench "b8" $ nf B8.last s0
      , bench "list" $ nf L.last l0
      ],
      bgroup "map" [
        bench "text" $ nf (T.map f) t0
      , bench "b8" $ nf (B8.map f) s0
      , bench "list" $ nf (L.map f) l0
      ],
      bgroup "2map" [
        bench "text" $ nf (T.map f . T.map f) t0
      , bench "b8" $ nf (B8.map f . B8.map f) s0
      , bench "list" $ nf (L.map f . L.map f) l0
      ],
      bgroup "reverse" [
        bench "text" $ nf T.reverse t0
      , bench "b8" $ nf B8.reverse s0
      , bench "list" $ nf L.reverse l0
      ],
      bgroup "take" [
        bench "text" $ nf (T.take (t0l `div` 3)) t0
      , bench "b8" $ nf (B8.take (s0l `div` 3)) s0
      , bench "list" $ nf (L.take (l0l `div` 3)) l0
      ],
      bgroup "words" [
        bench "text" $ nf T.words t0
      , bench "b8" $ nf B8.words s0
      , bench "list" $ nf L.words l0
      ],
      bgroup "zipWith" [
        bench "text" $ nf (T.zipWith min t1) t0
      , bench "b8" $ nf (B8.zipWith min s1) s0
      , bench "list" $ nf (L.zipWith min l1) l0
      ],
      bgroup "length" [
        bgroup "decode" [
          bench "text" $ nf (T.length . decodeUtf8) s0
        , bench "b8" $ nf (L.length . B8.unpack) s0
        , bench "utf8-string" $ nf (L.length . UTF8.toString) s0
        ],
        bgroup "drop" [
          bench "text" $ nf (T.length . T.drop (t0l `div` 3)) t0
        , bench "b8" $ nf (B8.length . B8.drop (s0l `div` 3)) s0
        , bench "list" $ nf (L.length . L.drop (l0l `div` 3)) l0
        ],
        bgroup "filter" [
          bench "text" $ nf (T.length . T.filter p0) t0
        , bench "b8" $ nf (B8.length . B8.filter p0) s0
        , bench "list" $ nf (L.length . L.filter p0) l0
        ],
        bgroup "2filter" [
          bench "text" $ nf (T.length . T.filter p1 . T.filter p0) t0
        , bench "b8" $ nf (B8.length . B8.filter p1 . B8.filter p0) s0
        , bench "list" $ nf (L.length . L.filter p1 . L.filter p0) l0
        ],
        bgroup "map" [
          bench "text" $ nf (T.length . T.map f) t0
        , bench "b8" $ nf (B8.length . B8.map f) s0
        , bench "list" $ nf (L.length . L.map f) l0
        ],
        bgroup "2map" [
          bench "text" $ nf (T.length . T.map f . T.map f) t0
        , bench "b8" $ nf (B8.length . B8.map f . B8.map f) s0
        , bench "list" $ nf (L.length . L.map f . L.map f) l0
        ],
        bgroup "take" [
          bench "text" $ nf (T.length . T.take (t0l `div` 3)) t0
        , bench "b8" $ nf (B8.length . B8.take (s0l `div` 3)) s0
        , bench "list" $ nf (L.length . L.take (l0l `div` 3)) l0
        ],
        bgroup "words" [
          bench "text" $ nf (L.length . T.words) t0
        , bench "b8" $ nf (L.length . B8.words) s0
        , bench "list" $ nf (L.length . L.words) l0
        ],
        bgroup "zipWith" [
          bench "text" $ nf (T.length . T.zipWith min t1) t0
        , bench "b8" $ nf (L.length . B8.zipWith min s1) s0
        , bench "list" $ nf (L.length . L.zipWith min l1) l0
        ]
      ]
    ]
  where
    p0 = (== 'й')
    p1 = (/= 'д')
    lw  = "право"
    sw  = UTF8.fromString lw
    tw  = T.pack lw
    f (C# c#) = C# (chr# (ord# c# +# 1#))
    len l _ = l + (1::Int)
