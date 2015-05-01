module Arsec
    (
      Comment
    , comment
    , semi
    , showX
    , key
    , value
    , unichar
    , unichars
    , module Control.Applicative
    , module Control.Monad
    , module Data.Char
    , module Text.ParserCombinators.Parsec.Char
    , module Text.ParserCombinators.Parsec.Combinator
    , module Text.ParserCombinators.Parsec.Error
    , module Text.ParserCombinators.Parsec.Prim
    ) where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.List (intercalate)
import Numeric
import Text.ParserCombinators.Parsec.Char hiding (lower, upper)
import Text.ParserCombinators.Parsec.Combinator hiding (optional)
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Prim hiding ((<|>), many)

type Comment = String

unichar :: Parser Char
unichar = chr . fst . head . readHex <$> many1 hexDigit

unichars :: Parser [Char]
unichars = manyTill (unichar <* spaces) semi

semi :: Parser ()
semi = char ';' *> spaces *> pure ()

comment :: Parser Comment
comment = (char '#' *> manyTill anyToken (char '\n')) <|> string "\n"

showX :: Char -> String
showX c
  | c > '\xffff' = error $ "U+" ++ h ++ " will not fit in 16 bits"
  | otherwise    = "0x" ++ replicate (4 - length h) '0' ++ h
    where h = showHex (ord c) ""

key :: (t -> Char) -> (t -> String) -> t -> String
key code name c = "  " ++ showX (code c) ++ "," ++ "  /* " ++ name c ++ " */"

value :: (t -> Char) -> (t -> String) -> (t -> String) -> t -> String
value code name mapping c =
    "  /* " ++ showX (code c) ++ " - " ++ name c ++ " */\n" ++
    "  {" ++ intercalate ", " xs ++ "},"
  where xs = take 3 (map showX (mapping c) ++ ["0","0"])
