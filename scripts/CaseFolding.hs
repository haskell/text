-- This script processes the following source file:
--
--   http://unicode.org/Public/UNIDATA/CaseFolding.txt

module CaseFolding
    (
      CaseFolding(..)
    , Fold(..)
    , parseCF
    , mapCF
    ) where

import Arsec
import Data.Bits

data Fold = Fold {
      code :: Char
    , status :: Char
    , mapping :: [Char]
    , name :: String
    } deriving (Eq, Ord, Show)

data CaseFolding = CF { cfComments :: [Comment], cfFolding :: [Fold] }
                 deriving (Show)

entries :: Parser CaseFolding
entries = CF <$> many comment <*> many (entry <* many comment)
  where
    entry = Fold <$> unichar <* semi
                 <*> oneOf "CFST" <* semi
                 <*> unichars
                 <*> (string "# " *> manyTill anyToken (char '\n'))

parseCF :: FilePath -> IO (Either ParseError CaseFolding)
parseCF name = parse entries name <$> readFile name

mapCF :: CaseFolding -> [String]
mapCF (CF _ ms) = typ ++ map printUnusual (filter (\f -> status f `elem` "CF") ms) ++ [last]
  where
    typ = ["foldMapping :: Char# -> _ {- unboxed Int64 -}"
           ,"{-# NOINLINE foldMapping #-}"
           ,"foldMapping = \\case"]
    last = "  _ -> unI64 0"
    printUnusual c = "  -- " ++ name c ++ "\n" ++
             "  " ++ showC (code c) ++ "# -> unI64 "  ++ show (ord x + (ord y `shiftL` 21) + (ord z `shiftL` 42))
       where x:y:z:_ = mapping c ++ repeat '\0'
