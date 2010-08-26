module TestUtils
    (
      withTempFile
    ) where

import Control.Exception (bracket)
import Control.Monad (when)
import System.Directory (removeFile)
import System.IO (Handle, hClose, hIsOpen, openTempFile)

withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile = bracket (openTempFile "." "crashy.txt") cleanupTemp . uncurry
  where
    cleanupTemp (path,h) = do
      open <- hIsOpen h
      when open (hClose h)
      removeFile path
