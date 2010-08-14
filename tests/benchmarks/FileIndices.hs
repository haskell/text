{-# LANGUAGE BangPatterns #-}

import System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Search as B

text :: FilePath -> String -> IO ()
text file pat = T.readFile file >>= print . T.count (T.pack pat)

textBS :: FilePath -> String -> IO ()
textBS file pat = B.readFile file >>= print . T.count (T.pack pat) . T.decodeUtf8

bytestring :: FilePath -> String -> IO ()
bytestring file pat = B.readFile file >>= print . length . B.indices (BS.pack pat)

main = do
  (name : file : pat : _) <- getArgs
  case name of
    "bs" -> bytestring file pat
    "text" -> text file pat
    "textBS" -> textBS file pat
