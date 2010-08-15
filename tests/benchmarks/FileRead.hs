{-# LANGUAGE BangPatterns #-}

import System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Search as B

string :: FilePath -> IO ()
string file = readFile file >>= print . length

text :: FilePath -> IO ()
text file = T.readFile file >>= print . T.length

textBS :: FilePath -> IO ()
textBS file = B.readFile file >>= print . T.length . T.decodeUtf8

bytestring :: FilePath -> IO ()
bytestring file = B.readFile file >>= print . B.length

main = do
  (name : file : _) <- getArgs
  case name of
    "bs" -> bytestring file
    "string" -> string file
    "text" -> text file
    "textBS" -> textBS file
