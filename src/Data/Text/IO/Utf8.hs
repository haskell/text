-- |
-- Module      : Data.Text.IO.Utf8
-- License     : BSD-style
-- Portability : GHC
--
-- Efficient UTF-8 support for text I\/O.
-- Unlike @Data.Text.IO@, these functions do not depend on the locale
-- and do not do line ending conversion.
module Data.Text.IO.Utf8
    (
    -- * File-at-a-time operations
      readFile
    , writeFile
    , appendFile
    -- * Operations on handles
    , hGetContents
    , hGetLine
    , hPutStr
    , hPutStrLn
    -- * Special cases for standard input and output
    , interact
    , getContents
    , getLine
    , putStr
    , putStrLn
    ) where

import Prelude hiding (readFile, writeFile, appendFile, interact, getContents, getLine, putStr, putStrLn)
import Control.Exception (evaluate)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.IO.Handle (Handle)
import qualified Data.ByteString.Char8 as B.Char8

decodeUtf8IO :: ByteString -> IO Text
decodeUtf8IO = evaluate . decodeUtf8

-- | The 'readFile' function reads a file and returns the contents of
-- the file as a string.  The entire file is read strictly, as with
-- 'getContents'.
readFile :: FilePath -> IO Text
readFile = decodeUtf8IO <=< B.readFile

-- | Write a string to a file.  The file is truncated to zero length
-- before writing begins.
writeFile :: FilePath -> Text -> IO ()
writeFile fp = B.writeFile fp . encodeUtf8

-- | Write a string to the end of a file.
appendFile :: FilePath -> Text -> IO ()
appendFile fp = B.appendFile fp . encodeUtf8

-- | Read the remaining contents of a 'Handle' as a string.
hGetContents :: Handle -> IO Text
hGetContents = decodeUtf8IO <=< B.hGetContents

-- | Read a single line from a handle.
hGetLine :: Handle -> IO Text
hGetLine = decodeUtf8IO <=< B.hGetLine

-- | Write a string to a handle.
hPutStr :: Handle -> Text -> IO ()
hPutStr h = B.hPutStr h . encodeUtf8

-- | Write a string to a handle, followed by a newline.
hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn h t = hPutStr h t >> B.hPutStr h (B.Char8.singleton '\n')

-- | The 'interact' function takes a function of type @Text -> Text@
-- as its argument. The entire input from the standard input device is
-- passed to this function as its argument, and the resulting string
-- is output on the standard output device.
interact :: (Text -> Text) -> IO ()
interact f = putStr . f =<< getContents

-- | Read all user input on 'stdin' as a single string.
getContents :: IO Text
getContents = decodeUtf8IO =<< B.getContents

-- | Read a single line of user input from 'stdin'.
getLine :: IO Text
getLine = decodeUtf8IO =<< B.getLine

-- | Write a string to 'stdout'.
putStr :: Text -> IO ()
putStr = B.putStr . encodeUtf8

-- | Write a string to 'stdout', followed by a newline.
putStrLn :: Text -> IO ()
putStrLn t = B.putStr (encodeUtf8 t) >> B.putStr (B.Char8.singleton '\n')
