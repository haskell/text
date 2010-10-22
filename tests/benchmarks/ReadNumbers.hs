{-# LANGUAGE BangPatterns #-}
import Debug.Trace
import Control.Monad
import Data.List
import qualified Data.Text.Read as T
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import System.Environment

dec = T.signed T.decimal :: T.Reader Int

hex = T.signed T.hexadecimal :: T.Reader Int

double = T.double :: T.Reader Double

rational = T.rational :: T.Reader Double

read1 :: (Ord a, Num a) => T.Reader a -> T.Text -> a
read1 reader = foldl' go 1000000 . T.lines
  where go z t = case reader t of
                   Left err    -> error err
                   Right (n,_) -> min n z
    
--paranoid :: T.Text -> [(Double)]
paranoid = foldr go [] . T.lines
  where go t xs = let a = case T.double t of
                           Left err    -> error err
                           Right (n,_) -> n
                      b = case T.rational t of
                            Left err    -> error err
                            Right (n,_) -> n
                 in if a /= b
                    then abs ((a-b)/a) : xs
                    else xs
    
read2 :: Num a => T.Reader a -> T.Text -> a
read2 reader = go 0
 where
   go !i t
    | T.null t = 0
    | otherwise = case reader t of
                    Left err -> error err
                    Right (n,t') -> case T.uncons t' of
                                      Nothing -> n
                                      Just (_,t'') -> go (n+i) t''

main = do
  args <- getArgs
  t <- T.decodeUtf8 `fmap` B.getContents
  case args of
    ["dec"] -> print . read1 dec $ t
    ["hex"] -> print . read1 hex $ t
    ["double"] -> print . read1 double $ t
    ["rational"] -> print . read1 rational $ t
  --mapM_ print =<< paranoid `fmap` T.getContents
