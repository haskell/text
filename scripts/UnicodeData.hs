-- This script processes the following source file:
--
--   http://unicode.org/Public/UNIDATA/UnicodeData.txt
--
-- Format description: https://www.unicode.org/reports/tr44/tr44-36.html#UnicodeData.txt

module UnicodeData
    ( UnicodeData
    , Data(..)
    , toTitleUD
    , parseUD
    ) where

import Debug.Trace
import Arsec hiding (semi)
import Data.Array
import Data.Functor (void)
import Data.List (sort)
import Data.Maybe (fromMaybe)

type UnicodeData = Array Int Data

-- "Simple_Titlecase_Mapping: If this field is null, then the Simple_Titlecase_Mapping
-- is the same as the Simple_Uppercase_Mapping for this character."
-- -- https://www.unicode.org/reports/tr44/tr44-36.html#UnicodeData.txt
toTitleUD :: Data -> Maybe Char
toTitleUD d = toTitleUD_ d <|> toUpperUD d

data Data = Data {
      charUD :: {-# UNPACK #-} !Char
    , toUpperUD :: {-# UNPACK #-} !(Maybe Char)
    , toLowerUD :: {-# UNPACK #-} !(Maybe Char)
    , toTitleUD_ :: {-# UNPACK #-} !(Maybe Char)
    } deriving (Eq, Ord, Show)

-- I'm pretty sure UnicodeData.txt is sorted but still sort it to be 100% certain.
entries :: Parser UnicodeData
entries = (\xs -> listArray (0, length xs - 1) xs) <$> many entry <* eof
  where
    entry = Data <$> unichar <* semi
                <* replicateM_ 11 (ignoreField <* semi)
                <*> optional unichar <* semi
                <*> optional unichar <* semi
                <*> optional unichar <* char '\n'
    semi = char ';'

ignoreField :: Parser ()
ignoreField = void (many (satisfy (\c -> c /= ';')))

parseUD :: FilePath -> IO (Either ParseError UnicodeData)
parseUD name = parse entries name <$> readFile name
