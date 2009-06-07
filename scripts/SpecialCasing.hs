-- This script processes the following source file:
--
--   http://unicode.org/Public/UNIDATA/SpecialCasing.txt

module SpecialCasing
    (
      Case(..)
    , parseSC
    , mapSC
    ) where

import Arsec

data Case = Case {
      code :: Char
    , lower :: [Char]
    , title :: [Char]
    , upper :: [Char]
    , conditions :: String
    , name :: String
    } deriving (Eq, Ord, Show)

entries :: Parser [Case]
entries = many comment *> many (entry <* many comment)
  where
    entry = Case <$> unichar <* semi
                 <*> unichars
                 <*> unichars
                 <*> unichars
                 <*> manyTill anyToken (string "# ")
                 <*> manyTill anyToken (char '\n')

parseSC :: FilePath -> IO (Either ParseError [Case])
parseSC name = parse entries name <$> readFile name

mapSC :: String -> (Case -> String) -> (Char -> Char) -> [Case] -> [String]
mapSC which access twiddle ms = typ ++ (map nice . filter p $ ms) ++ [last]
  where
    typ = [which ++ "Mapping :: forall s. Char -> s -> Step (PairS (PairS s Char) Char) Char"
           ,"{-# INLINE " ++ which ++ "Mapping #-}"]
    last = which ++ "Mapping c s = Yield (to" ++ ucFirst which ++ " c) (s :!: '\\0' :!: '\\0')"
    nice c = "-- " ++ name c ++ "\n" ++
             which ++ "Mapping " ++ showC (code c) ++ " s = Yield " ++ x ++ " (s :!: " ++ y ++ " :!: " ++ z ++ ")"
       where [x,y,z] = (map showC . take 3) (access c ++ repeat '\0')
    p c = [k] /= a && a /= [twiddle k] && null (conditions c)
        where a = access c
              k = code c

ucFirst (c:cs) = toUpper c : cs
ucFirst [] = []
