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
import Data.List (sort)

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
mapCF (CF _ ms) =
    ["const uint16_t _hs_text_to_case_fold_keys[" ++ count ++ "] = {"] ++
    map (key code name) mappings ++
    ["};"
    ,"const size_t _hs_text_to_case_fold_len = " ++ count ++ ";"
    ,"const uint16_t _hs_text_to_case_fold_values[" ++ count ++ "][3] = {"] ++
    map (value code name mapping) mappings ++
    ["};"]
  where
    count = show (length mappings)
    mappings = sort (filter p ms)
    p f = status f `elem` "CF" &&
          mapping f /= [toLower (code f)]
