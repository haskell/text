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
import Data.Bits

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
    typ ++ map printUnusual ms' ++ map printUsual usual ++ [last]
  where
    ms' = filter p ms
    p c = [k] /= a && a /= [twiddle k] && null (conditions c)
        where a = access c
              k = code c
    unusual = map code ms'
    usual = filter (\c -> twiddle c /= c && c `notElem` unusual) [minBound..maxBound]

    typ = [which ++ "Mapping :: Char# -> _ {- unboxed Int64 -}"
           ,"{-# NOINLINE " ++ which ++ "Mapping #-}"
           ,which ++ "Mapping = \\case"]
    last = "  _ -> unI64 0"
    printUnusual c = "  -- " ++ name c ++ "\n" ++
             "  " ++ showC (code c) ++ "# -> unI64 " ++ show (ord x + (ord y `shiftL` 21) + (ord z `shiftL` 42))
       where x:y:z:_ = access c ++ repeat '\0'
    printUsual c = "  " ++ showC c ++ "# -> unI64 " ++ show (ord (twiddle c))

ucFirst (c:cs) = toUpper c : cs
ucFirst [] = []
