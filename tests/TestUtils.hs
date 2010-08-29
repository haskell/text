module TestUtils
    (
      withRedirect
    , withTempFile
    ) where

import Control.Exception (bracket, bracket_)
import Control.Monad (when)
import GHC.IO.Handle.Internals (withHandle)
import System.Directory (removeFile)
import System.IO

withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile = bracket (openTempFile "." "crashy.txt") cleanupTemp . uncurry
  where
    cleanupTemp (path,h) = do
      open <- hIsOpen h
      when open (hClose h)
      removeFile path

withRedirect :: Handle -> Handle -> IO a -> IO a
withRedirect tmp h = bracket_ swap swap
  where
    whenM p a = p >>= (`when` a)
    swap = do
      whenM (hIsOpen tmp) $ whenM (hIsWritable tmp) $ hFlush tmp
      whenM (hIsOpen h) $ whenM (hIsWritable h) $ hFlush h
      withHandle "spam" tmp $ \tmph -> do
        hh <- withHandle "spam" h $ \hh ->
          return (tmph,hh)
        return (hh,())
