{-# LANGUAGE OverloadedStrings #-}
import Data.Text as T
import Data.ByteString as B
import Data.Text.Encoding as T

main = do
  let k = 50000000
      t = T.replicate k "a"
  B.putStr (T.encodeUtf8 t)
