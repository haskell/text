{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Codec.Binary.UTF8.Generic as U8
import System.Environment
import System.IO

strict_bytestring k s = do
  let t = T.replicate k (T.pack s)
  B.putStr (T.encodeUtf8 t)

lazy_bytestring k s = do
  let t = TL.replicate (fromIntegral k) (TL.pack s)
  BL.putStr (TL.encodeUtf8 t)

strict_io k s = do
  let t = T.replicate k (T.pack s)
  hSetEncoding stdout utf8
  T.putStr t

lazy_io k s = do
  let t = TL.replicate (fromIntegral k) (TL.pack s)
  hSetEncoding stdout utf8
  TL.putStr t

string k s = do
  let t = concat $ replicate k s
  hSetEncoding stdout utf8
  putStr t

lazy_string_utf8 k s = do
  let t = concat $ replicate k s
  BL.putStr (U8.fromString t)

strict_string_utf8 k s = do
  let t = concat $ replicate k s
  B.putStr (U8.fromString t)

main = do
  [kind,str,kstr] <- getArgs
  let k = read kstr * 1000000
  case kind of
    "strict" -> strict_bytestring k str
    "lazy" -> lazy_bytestring k str
    "strict_io" -> strict_io k str
    "lazy_io" -> lazy_io k str
    "string" -> string k str
    "lazy_string_utf8" -> lazy_string_utf8 k str
    "strict_string_utf8" -> strict_string_utf8 k str
