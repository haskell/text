{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Environment (getArgs)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.ByteString.Lazy.Search as LB
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B

lazyText file pat sub =
  LT.readFile file >>= LT.putStr . LT.replace (LT.pack pat) (LT.pack sub)

lazyBS file pat sub =
  LB.readFile file >>= LB.putStr . LB.replace (B.pack pat) (LB.pack sub)

main = do
  (kind : file : pat : sub : _) <- getArgs
  case kind of
    "lazyText" -> lazyText file pat sub
    "lazyTextNull" -> LT.readFile file >>= LT.putStr
    "lazyBS" -> lazyBS file pat sub
    "lazyBSNull" -> LB.readFile file >>= LB.putStr
