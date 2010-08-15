{-# LANGUAGE BangPatterns #-}

import System.Environment (getArgs)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Search as B
import System.IO

string :: Handle -> IO ()
string h = hGetContents h >>= print . length

text :: Handle -> IO ()
text h = do
  t <- {-# SCC "T.hGetContents" #-} T.hGetContents h
  print (T.length t)

textBS :: Handle -> IO ()
textBS h = do
  bs <- {-# SCC "B.hGetContents" #-} B.hGetContents h
  print . T.length . T.decodeUtf8 $ bs

bytestring :: Handle -> IO ()
bytestring h = do
  bs <- {-# SCC "B.hGetContents" #-} B.hGetContents h
  print (B.length bs)

main = do
  (name : file : _) <- getArgs
  h <- openFile file ReadMode
  hSetBuffering h (BlockBuffering (Just 16384))
  case name of
    "bs" -> bytestring h
    "string" -> string h
    "text" -> text h
    "textBS" -> textBS h
