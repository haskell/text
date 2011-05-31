-- | Program to replace everything between brackets by spaces
--
-- This program was originally contributed by Petr Prokhorenkov.
--
module Data.Text.Benchmarks.StripBrackets
    ( benchmark
    ) where
     
import Criterion (Benchmark, bench)
import System.IO (Handle)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

benchmark :: FilePath -> Handle -> IO Benchmark
benchmark fp sink = return $ bench "StripBrackets" $ do
    t <- T.decodeUtf8 `fmap` B.readFile fp
    B.hPutStr sink $ T.encodeUtf8 $ stripBrackets t

stripBrackets :: T.Text -> T.Text
stripBrackets = snd . T.mapAccumL f (0 :: Int)
  where
    f depth c =
        let depth' = depth + d' c
            c' | depth > 0 || depth' > 0 = ' '
               | otherwise = c
        in (depth', c')

    d' '{' = 1
    d' '[' = 1
    d' '}' = -1
    d' ']' = -1
    d' _   = 0
