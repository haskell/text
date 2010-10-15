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

strict_len h = do
  bs <- B.hGetContents h
  print . T.length . T.decodeUtf8 $ bs

strict_init_len h = do
  bs <- B.hGetContents h
  print . T.length . T.init . T.decodeUtf8 $ bs

strict_io h = do
  hSetEncoding h utf8
  t <- T.hGetContents h
  rnf t `seq` return ()

strict_len_io h = do
  hSetEncoding h utf8
  t <- T.hGetContents h
  print (T.length t)

lazy h = do
  bs <- BL.hGetContents h
  rnf (TL.decodeUtf8 bs) `seq` return ()

lazy_len h = do
  bs <- BL.hGetContents h
  print . TL.length . TL.decodeUtf8 $ bs

lazy_init_len h = do
  bs <- BL.hGetContents h
  print . TL.length . TL.init . TL.decodeUtf8 $ bs

lazy_io h = do
  hSetEncoding h utf8
  t <- TL.hGetContents h
  rnf t `seq` return ()

lazy_len_io h = do
  hSetEncoding h utf8
  t <- TL.hGetContents h
  print (TL.length t)

string h = do
  hSetEncoding h utf8
  t <- hGetContents h
  rnf t `seq` return ()

string_len h = do
  hSetEncoding h utf8
  t <- hGetContents h
  print (length t)

lazy_string_utf8 h = do
  bs <- BL.hGetContents h
  let t = U8.toString bs
  rnf t `seq` return ()

lazy_string_utf8_len h = do
  bs <- BL.hGetContents h
  let t = U8.toString bs
  print (length t)

strict_string_utf8 h = do
  bs <- B.hGetContents h
  let t = U8.toString bs
  rnf t `seq` return ()

strict_string_utf8_len h = do
  bs <- B.hGetContents h
  let t = U8.toString bs
  print (length t)

main = do
  [kind,name] <- getArgs
  h <- openFile name ReadMode
  case kind of
    "strict" -> strict h
    "strict_len" -> strict_len h
    "strict_init_len" -> strict_init_len h
    "strict_io" -> strict_io h
    "strict_len_io" -> strict_len_io h
    "lazy" -> lazy h
    "lazy_len" -> lazy_len h
    "lazy_init_len" -> lazy_init_len h
    "lazy_io" -> lazy_io h
    "lazy_len_io" -> lazy_len_io h
    "string" -> string h
    "string_len" -> string_len h
    "lazy_string_utf8" -> lazy_string_utf8 h
    "lazy_string_utf8_len" -> lazy_string_utf8_len h
    "strict_string_utf8" -> strict_string_utf8 h
    "strict_string_utf8_len" -> strict_string_utf8_len h
