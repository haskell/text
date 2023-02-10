-- |
-- Module      : Data.Text.IO.Utf8
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Simon Marlow
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- Efficient UTF-8 support for text I\/O.
module Data.Text.IO.Utf8
    (
      readFile
    , writeFile
    , appendFile
    ) where

import Prelude hiding (readFile, writeFile, appendFile)
import Control.Exception (evaluate)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

decodeUtf8IO :: ByteString -> IO Text
decodeUtf8IO = evaluate . decodeUtf8

encodeUtf8IO :: Text -> IO ByteString
encodeUtf8IO = evaluate . encodeUtf8

-- | The 'readFile' function reads a file and returns the contents of
-- the file as a string.  The entire file is read strictly, as with
-- 'getContents'.
readFile :: FilePath -> IO Text
readFile = decodeUtf8IO <=< B.readFile

-- | Write a string to a file.  The file is truncated to zero length
-- before writing begins.
writeFile :: FilePath -> Text -> IO ()
writeFile fp = B.writeFile fp <=< encodeUtf8IO

-- | Write a string to the end of a file.
appendFile :: FilePath -> Text -> IO ()
appendFile fp = B.appendFile fp <=< encodeUtf8IO
