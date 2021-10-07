{-# LANGUAGE BangPatterns, CPP, RecordWildCards #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module      : Data.Text.Lazy.IO
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Simon Marlow
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- Efficient locale-sensitive support for lazy text I\/O.
--
-- The functions in this module obey the runtime system's locale,
-- character set encoding, and line ending conversion settings.
--
-- If you know in advance that you will be working with data that has
-- a specific encoding (e.g. UTF-8), and your application is highly
-- performance sensitive, you may find that it is faster to perform
-- I\/O with bytestrings and to encode and decode yourself than to use
-- the functions in this module.

module Data.Text.Lazy.IO
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

import Data.Text.Lazy (Text)
import Prelude hiding (appendFile, getContents, getLine, interact,
                       putStr, putStrLn, readFile, writeFile)
import System.IO (Handle, IOMode(..), hPutChar, openFile, stdin, stdout,
                  withFile)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Control.Exception as E
import Control.Monad (when)
import Data.IORef (readIORef)
import Data.Text.Internal.IO (hGetLineWith, readChunk)
import Data.Text.Internal.Lazy (chunk, empty)
import GHC.IO.Buffer (isEmptyBuffer)
import GHC.IO.Exception (IOException(..), IOErrorType(..), ioException)
import GHC.IO.Handle.Internals (augmentIOError, hClose_help,
                                wantReadableHandle, withHandle)
import GHC.IO.Handle.Types (Handle__(..), HandleType(..))
import System.IO (BufferMode(..), hGetBuffering, hSetBuffering)
import System.IO.Error (isEOFError)
import System.IO.Unsafe (unsafeInterleaveIO)

-- | Read a file and return its contents as a string.  The file is
-- read lazily, as with 'getContents'.
--
-- Beware that this function (similarly to 'Prelude.readFile') is locale-dependent.
-- Unexpected system locale may cause your application to read corrupted data or
-- throw runtime exceptions about "invalid argument (invalid byte sequence)"
-- or "invalid argument (invalid character)". This is also slow, because GHC
-- first converts an entire input to UTF-32, which is afterwards converted to UTF-8.
--
-- If your data is UTF-8,
-- using 'Data.Text.Lazy.Encoding.decodeUtf8' '.' 'Data.ByteString.Lazy.readFile'
-- is a much faster and safer alternative.
readFile :: FilePath -> IO Text
readFile name = openFile name ReadMode >>= hGetContents

-- | Write a string to a file.  The file is truncated to zero length
-- before writing begins.
writeFile :: FilePath -> Text -> IO ()
writeFile p = withFile p WriteMode . flip hPutStr

-- | Write a string to the end of a file.
appendFile :: FilePath -> Text -> IO ()
appendFile p = withFile p AppendMode . flip hPutStr

-- | Lazily read the remaining contents of a 'Handle'.  The 'Handle'
-- will be closed after the read completes, or on error.
hGetContents :: Handle -> IO Text
hGetContents h = do
  chooseGoodBuffering h
  wantReadableHandle "hGetContents" h $ \hh -> do
    ts <- lazyRead h
    return (hh{haType=SemiClosedHandle}, ts)

-- | Use a more efficient buffer size if we're reading in
-- block-buffered mode with the default buffer size.
chooseGoodBuffering :: Handle -> IO ()
chooseGoodBuffering h = do
  bufMode <- hGetBuffering h
  when (bufMode == BlockBuffering Nothing) $
    hSetBuffering h (BlockBuffering (Just 16384))

lazyRead :: Handle -> IO Text
lazyRead h = unsafeInterleaveIO $
  withHandle "hGetContents" h $ \hh -> do
    case haType hh of
      ClosedHandle     -> return (hh, L.empty)
      SemiClosedHandle -> lazyReadBuffered h hh
      _                -> ioException
                          (IOError (Just h) IllegalOperation "hGetContents"
                           "illegal handle type" Nothing Nothing)

lazyReadBuffered :: Handle -> Handle__ -> IO (Handle__, Text)
lazyReadBuffered h hh@Handle__{..} = do
   buf <- readIORef haCharBuffer
   (do t <- readChunk hh buf
       ts <- lazyRead h
       return (hh, chunk t ts)) `E.catch` \e -> do
         (hh', _) <- hClose_help hh
         if isEOFError e
           then return $ if isEmptyBuffer buf
                         then (hh', empty)
                         else (hh', L.singleton '\r')
           else E.throwIO (augmentIOError e "hGetContents" h)

-- | Read a single line from a handle.
hGetLine :: Handle -> IO Text
hGetLine = hGetLineWith L.fromChunks

-- | Write a string to a handle.
hPutStr :: Handle -> Text -> IO ()
hPutStr h = mapM_ (T.hPutStr h) . L.toChunks

-- | Write a string to a handle, followed by a newline.
hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn h t = hPutStr h t >> hPutChar h '\n'

-- | The 'interact' function takes a function of type @Text -> Text@
-- as its argument. The entire input from the standard input device is
-- passed (lazily) to this function as its argument, and the resulting
-- string is output on the standard output device.
interact :: (Text -> Text) -> IO ()
interact f = putStr . f =<< getContents

-- | Lazily read all user input on 'stdin' as a single string.
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
