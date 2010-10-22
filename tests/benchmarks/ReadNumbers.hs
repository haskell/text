{-# LANGUAGE BangPatterns #-}

import Data.List (foldl')
import System.Environment (getArgs)
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

readem :: (Ord a, Num a) =>
          IO [t] -> (t -> Either String (a,t)) -> IO ()
readem act reader = print . foldl' go 1000000 =<< act
  where go z t = case reader t of
                   Left err    -> error err
                   Right (n,_) -> min n z
    
bytey :: (Ord a, Num a) =>
         IO [t] -> (t -> Maybe (a,t)) -> IO ()
bytey act reader = print . foldl' go 1000000 =<< act
  where go z t = case reader t of
                   Nothing    -> error "barf"
                   Just (n,_) -> min n z

main = do
  args <- getArgs
  let strict = (T.lines . T.decodeUtf8) `fmap` B.getContents
      lazy = (TL.lines . TL.decodeUtf8) `fmap` BL.getContents
      bs = B.lines `fmap` B.getContents
      lbs = BL.lines `fmap` BL.getContents
  case args of
    ["dec"] -> readem strict (T.signed T.decimal :: T.Reader Int)
    ["hex"] -> readem strict (T.signed T.hexadecimal :: T.Reader Int)
    ["double"] -> readem strict (T.double :: T.Reader Double)
    ["rational"] -> readem strict (T.rational :: T.Reader Double)
    ["ldec"] -> readem lazy (TL.signed TL.decimal :: TL.Reader Int)
    ["lhex"] -> readem lazy (TL.signed TL.hexadecimal :: TL.Reader Int)
    ["ldouble"] -> readem lazy (TL.double :: TL.Reader Double)
    ["lrational"] -> readem lazy (TL.rational :: TL.Reader Double)
    ["bdec"] -> bytey bs B.readInt
    ["bdouble"] -> bytey bs B.readDouble
    ["bldec"] -> bytey lbs BL.readInt
    ["bldouble"] -> bytey lbs BL.readDouble
