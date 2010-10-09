{-# LANGUAGE BangPatterns #-}
import Debug.Trace
import Control.Monad
import Data.List
import qualified Data.Text.Lex as T
import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.Environment

dec = T.signed T.decimal :: T.Lexer Int

hex = T.signed T.hexadecimal :: T.Lexer Int

double = T.double :: T.Lexer Double

def = double

read1 :: Num a => T.Lexer a -> T.Text -> a
read1 reader = foldl' go 0 . T.lines
  where go z t = case reader t of
                   Left err    -> error err
                   Right (n,_) -> n
    
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
    
read2 :: Num a => T.Lexer a -> T.Text -> a
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
  (print . read2 def) =<< T.getContents
  --mapM_ print =<< paranoid `fmap` T.getContents
