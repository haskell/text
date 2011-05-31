-- | Compare a string with a copy of itself that is identical except
-- for the last character.
--
module Data.Text.Benchmarks.Equality
    (
      benchmark
    ) where

import Criterion (Benchmark, bgroup, bench, whnf)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

benchmark :: FilePath -> IO Benchmark
benchmark fp = do
  b <- B.readFile fp
  bl <- BL.readFile fp
  l <- readFile fp
  let t  = T.decodeUtf8 b
      tl = TL.decodeUtf8 bl
  return $ bgroup "Equality"
    [ bench "Text" $ whnf (== T.init t `T.snoc` '\xfffd') t
    , bench "LazyText" $ whnf (== TL.init tl `TL.snoc` '\xfffd') tl
    , bench "ByteString" $ whnf (== B.init b `B.snoc` '\xfffd') b
    , bench "LazyByteString" $ whnf (== BL.init bl `BL.snoc` '\xfffd') bl
    , bench "String" $ whnf (== init l ++ "\xfffd") l
    ]
