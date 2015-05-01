-- This script processes the following source file:
--
--   http://unicode.org/Public/UNIDATA/SpecialCasing.txt

module SpecialCasing
    (
      SpecialCasing(..)
    , Case(..)
    , parseSC
    , mapSC
    ) where

import Arsec
import Data.List (sort)

data SpecialCasing = SC { scComments :: [Comment], scCasing :: [Case] }
                   deriving (Show)

data Case = Case {
      code :: Char
    , lower :: [Char]
    , title :: [Char]
    , upper :: [Char]
    , conditions :: String
    , name :: String
    } deriving (Eq, Ord, Show)

entries :: Parser SpecialCasing
entries = SC <$> many comment <*> many (entry <* many comment)
  where
    entry = Case <$> unichar <* semi
                 <*> unichars
                 <*> unichars
                 <*> unichars
                 <*> manyTill anyToken (string "# ")
                 <*> manyTill anyToken (char '\n')

parseSC :: FilePath -> IO (Either ParseError SpecialCasing)
parseSC name = parse entries name <$> readFile name

mapSC :: String -> (Case -> String) -> (Char -> Char) -> SpecialCasing
         -> [String]
mapSC which access twiddle (SC _ ms) =
    ["const uint16_t _hs_text_to_" ++ which ++ "_keys[" ++ count ++ "] = {"] ++
    map (key code name) mappings ++
    ["};"
    ,"const size_t _hs_text_to_" ++ which ++ "_len = " ++ count ++ ";"
    ,"const uint16_t _hs_text_to_" ++ which ++ "_values[" ++ count ++ "][3] = {"] ++
    map (value code name access) mappings ++
    ["};"]
  where
    count = show (length mappings)
    mappings = sort (filter p ms)
    p c = [k] /= a && a /= [twiddle k] && null (conditions c)
        where a = access c
              k = code c

ucFirst (c:cs) = toUpper c : cs
ucFirst [] = []
