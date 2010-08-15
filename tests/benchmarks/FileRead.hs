{-# LANGUAGE BangPatterns #-}

import System.Environment (getArgs)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import System.IO

string :: Handle -> IO ()
string h = hGetContents h >>= print . length

ltext :: Handle -> IO ()
ltext h = do
  t <- {-# SCC "TL.hGetContents" #-} TL.hGetContents h
  print (TL.length t)

ltextBS :: Handle -> IO ()
ltextBS h = do
  bs <- {-# SCC "B.hGetContents" #-} BL.hGetContents h
  print . TL.length . TL.decodeUtf8 $ bs

text :: Handle -> IO ()
text h = do
  t <- {-# SCC "T.hGetContents" #-} T.hGetContents h
  print (T.length t)

textBS :: Handle -> IO ()
textBS h = do
  bs <- {-# SCC "B.hGetContents" #-} B.hGetContents h
  print . T.length . T.decodeUtf8 $ bs

lbytestring :: Handle -> IO ()
lbytestring h = do
  bs <- {-# SCC "BL.hGetContents" #-} BL.hGetContents h
  print (BL.length bs)

bytestring :: Handle -> IO ()
bytestring h = do
  bs <- {-# SCC "B.hGetContents" #-} B.hGetContents h
  print (B.length bs)

main = do
  (name : file : _) <- getArgs
  h <- openFile file ReadMode
  case name of
    "bs" -> bytestring h
    "lbs" -> lbytestring h
    "ltext" -> ltext h
    "ltextBS" -> ltextBS h
    "string" -> string h
    "text" -> text h
    "textBS" -> textBS h
