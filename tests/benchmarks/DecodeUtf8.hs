import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Codec.Binary.UTF8.Generic as U8
import Control.DeepSeq
import System.Environment
import System.IO

strict h = do
  bs <- B.hGetContents h
  rnf (T.decodeUtf8 bs) `seq` return ()

strict_ h = do
  bs <- B.hGetContents h
  rnf (T.decodeUtf8' bs) `seq` return ()

strict_io h = do
  hSetEncoding h utf8
  t <- T.hGetContents h
  rnf t `seq` return ()

lazy h = do
  bs <- BL.hGetContents h
  rnf (TL.decodeUtf8 bs) `seq` return ()

lazy_io h = do
  hSetEncoding h utf8
  t <- TL.hGetContents h
  rnf t `seq` return ()

string h = do
  hSetEncoding h utf8
  t <- hGetContents h
  rnf t `seq` return ()

main = do
  [kind,name] <- getArgs
  h <- openFile name ReadMode
  case kind of
    "strict" -> strict h
    "strict_" -> strict_ h
    "strict_io" -> strict_io h
    "lazy" -> lazy h
    "lazy_io" -> lazy_io h
    "string" -> string h
