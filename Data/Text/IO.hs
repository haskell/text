{-# LANGUAGE BangPatterns, CPP #-}
-- |
-- Module      : Data.Text.IO
-- Copyright   : (c) Bryan O'Sullivan 2009,
--               (c) Simon Marlow 2009
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Support for text I\/O.

module Data.Text.IO
    (
    -- * Locale support
    -- $locale
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

import Data.Text (Text)
import Prelude hiding (appendFile, getContents, getLine, interact, putStr,
                       putStrLn, readFile, writeFile)
import System.IO (Handle, hPutChar, stdin, stdout)
#if __GLASGOW_HASKELL__ <= 610
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
#else
import Data.IORef (readIORef, writeIORef)
import Data.Text.Fusion (stream)
import Data.Text.Fusion.Internal (Step(..), Stream(..))
import GHC.IO.Buffer (Buffer(..), BufferState(..), CharBufElem, CharBuffer,
                      RawCharBuffer, emptyBuffer, newCharBuffer, writeCharBuf)
import GHC.IO.Handle.Internals (wantWritableHandle)
import GHC.IO.Handle.Text (commitBuffer')
import GHC.IO.Handle.Types (BufferList(..), BufferMode(..), Handle__(..),
                            Newline(..))
import System.IO (IOMode(..), openFile, withFile)
#endif

-- | The 'readFile' function reads a file and returns the contents of
-- the file as a string.  The entire file is read strictly, as with
-- 'getContents'.
readFile :: FilePath -> IO Text
readFile name = openFile name ReadMode >>= hGetContents

-- | Write a string to a file.  The file is truncated to zero length
-- before writing begins.
writeFile :: FilePath -> Text -> IO ()
writeFile p = withFile p WriteMode . flip hPutStr

-- | Write a string the end of a file.
appendFile :: FilePath -> Text -> IO ()
appendFile p = withFile p AppendMode . flip hPutStr

-- | Read the remaining contents of a handle as a string.
hGetContents :: Handle -> IO Text
#if __GLASGOW_HASKELL__ <= 610
hGetContents = fmap decodeUtf8 . B.hGetContents
#else
hGetContents = undefined
#endif

-- | Read a single line from a handle.
hGetLine :: Handle -> IO Text
#if __GLASGOW_HASKELL__ <= 610
hGetLine = fmap decodeUtf8 . B.hGetLine
#else
hGetLine = undefined
#endif

-- | Write a string to a handle.
hPutStr :: Handle -> Text -> IO ()
#if __GLASGOW_HASKELL__ <= 610
hPutStr h = B.hPutStr h . encodeUtf8
#else
hPutStr h t = do
  (buffer_mode, nl) <- 
       wantWritableHandle "hPutStr" h $ \h_ -> do
                     bmode <- getSpareBuffer h_
                     return (bmode, haOutputNL h_)
  let str = stream t
  case buffer_mode of
     (NoBuffering, _)        -> hPutChars h str
     (LineBuffering, buf)    -> writeBlocks h True  nl buf str
     (BlockBuffering _, buf) -> writeBlocks h False nl buf str

hPutChars :: Handle -> Stream Char -> IO ()
hPutChars h (Stream next0 s0 _len) = loop s0
  where
    loop !s = case next0 s of
                Done       -> return ()
                Skip s'    -> loop s'
                Yield x s' -> hPutChar h x >> loop s'

-- This function is largely lifted from GHC.IO.Handle.Text, but
-- adapted to a coinductive stream of data instead of an inductive
-- list.
writeBlocks :: Handle -> Bool -> Newline -> Buffer CharBufElem -> Stream Char
            -> IO ()
writeBlocks h lineBuffered nl buf0 (Stream next0 s0 _len) = outer s0 buf0
 where
  outer s1 Buffer{bufRaw=raw, bufSize=len} = inner s1 (0::Int)
   where
    inner !s !n =
      case next0 s of
        Done -> commit n False{-no flush-} True{-release-} >> return ()
        Skip s' -> inner s' n
        Yield x s'
          | n + 1 >= len -> commit n True{-needs flush-} False >>= outer s
          | x == '\n'    -> do
                   n' <- if nl == CRLF
                         then do n1 <- writeCharBuf raw n '\r'
                                 writeCharBuf raw n1 '\n'
                         else writeCharBuf raw n x
                   if lineBuffered
                     then commit n' True{-needs flush-} False >>= outer s'
                     else inner s' n'
          | otherwise    -> writeCharBuf raw n x >>= inner s'
    commit = commitBuffer h raw len

-- This function is completely lifted from GHC.IO.Handle.Text.
getSpareBuffer :: Handle__ -> IO (BufferMode, CharBuffer)
getSpareBuffer Handle__{haCharBuffer=ref, 
                        haBuffers=spare_ref,
                        haBufferMode=mode}
 = do
   case mode of
     NoBuffering -> return (mode, error "no buffer!")
     _ -> do
          bufs <- readIORef spare_ref
          buf  <- readIORef ref
          case bufs of
            BufferListCons b rest -> do
                writeIORef spare_ref rest
                return ( mode, emptyBuffer b (bufSize buf) WriteBuffer)
            BufferListNil -> do
                new_buf <- newCharBuffer (bufSize buf) WriteBuffer
                return (mode, new_buf)


-- This function is completely lifted from GHC.IO.Handle.Text.
commitBuffer :: Handle -> RawCharBuffer -> Int -> Int -> Bool -> Bool
             -> IO CharBuffer
commitBuffer hdl !raw !sz !count flush release = 
  wantWritableHandle "commitAndReleaseBuffer" hdl $
     commitBuffer' raw sz count flush release
{-# NOINLINE commitBuffer #-}
#endif

-- | Write a string to a handle, followed by a newline.
hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn h t = hPutStr h t >> hPutChar h '\n'

-- | The 'interact' function takes a function of type @Text -> Text@
-- as its argument. The entire input from the standard input device is
-- passed to this function as its argument, and the resulting string
-- is output on the standard output device.
interact :: (Text -> Text) -> IO ()
interact f = putStr . f =<< getContents

-- | Read all user input on 'stdin' as a single string.
getContents :: IO Text
getContents = hGetContents stdin

-- | Read a single line of user input from 'stdin'.
getLine :: IO Text
getLine = hGetLine stdin

-- | Write a string to 'stdout'.
putStr :: Text -> IO ()
putStr = hPutStr stdout

-- | Write a string to 'stdout', followed by a newline.
putStrLn :: Text -> IO ()
putStrLn = hPutStrLn stdout

-- $locale
--
-- Under GHC 6.10 and earlier, the system I\/O libraries do not
-- support locale-sensitive I\/O.  All data read by functions in this
-- module is decoded as UTF-8, and before data is written, it is first
-- encoded as UTF-8.
--
-- Beginning with GHC 6.12, text I\/O is performed using the system or
-- handle's current locale.
