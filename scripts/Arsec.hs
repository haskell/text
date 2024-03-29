module Arsec
    (
      Comment
    , comment
    , semi
    , showC
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

import Prelude hiding (head, tail)
import Control.Monad
import Control.Applicative
import Data.Char
import Numeric (readHex, showHex)
import Text.ParserCombinators.Parsec.Char hiding (lower, upper)
import Text.ParserCombinators.Parsec.Combinator hiding (optional)
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Prim hiding ((<|>), many)

type Comment = String

unichar :: Parser Char
unichar = do
  digits <- many1 hexDigit
  case readHex digits of
    [] -> error "unichar: cannot parse hex digits"
    (hd, _) : _ -> pure $ chr hd

unichars :: Parser [Char]
unichars = manyTill (unichar <* spaces) semi

semi :: Parser ()
semi = char ';' *> spaces *> pure ()

comment :: Parser Comment
comment = (char '#' *> manyTill anyToken (char '\n')) <|> string "\n"

showC :: Char -> String
showC c = "'\\x" ++ d ++ "'"
    where h = showHex (ord c) ""
          d = replicate (4 - length h) '0' ++ h
