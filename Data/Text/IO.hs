-- |
-- Module      : Data.Text.IO
-- Copyright   : (c) Bryan O'Sullivan 2009
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Support for text I\/O.

module Data.Text.IO
    (
    -- * Operations on handles
      hGetContents
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

import Data.Text (Text)
import Prelude hiding (getContents, getLine, interact, putStr, putStrLn)
import System.IO (Handle, stdin, stdout)

hGetContents :: Handle -> IO Text
hGetContents h = undefined

hGetLine :: Handle -> IO Text
hGetLine h = undefined

hPutStr :: Handle -> Text -> IO ()
hPutStr h t = undefined

hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn h t = undefined

interact :: (Text -> Text) -> IO ()
interact f = undefined

getContents :: IO Text
getContents = hGetContents stdin

getLine :: IO Text
getLine = hGetLine stdin

putStr :: Text -> IO ()
putStr = hPutStr stdout

putStrLn :: Text -> IO ()
putStrLn = hPutStrLn stdout
