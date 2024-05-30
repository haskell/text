{-# LANGUAGE BangPatterns, CPP, RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module      : Data.Text.IO
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Simon Marlow
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- Efficient locale-sensitive support for text I\/O.
--
-- The functions in this module obey the runtime system's locale,
-- character set encoding, and line ending conversion settings.
--
-- If you want to do I\/O using the UTF-8 encoding, use @Data.Text.IO.Utf8@,
-- which is faster than this module.
--
-- If you know in advance that you will be working with data that has
-- a specific encoding, and your application is highly
-- performance sensitive, you may find that it is faster to perform
-- I\/O with bytestrings and to encode and decode yourself than to use
-- the functions in this module.

module Data.Text.IO
    (
    -- * File-at-a-time operations
      readFile
    , writeFile
    , appendFile
    -- * Operations on handles
    , hGetContents
    , hGetChunk
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
import Prelude hiding (appendFile, getContents, getLine, interact,
                       putStr, putStrLn, readFile, writeFile)
import System.IO (Handle, IOMode(..), openFile, stdin, stdout,
                  withFile)
import qualified Control.Exception as E
import Control.Monad (liftM2, when)
import Data.IORef (readIORef)
import qualified Data.Text as T
import Data.Text.Internal.Fusion (stream, streamLn)
import Data.Text.Internal.IO (hGetLineWith, readChunk, hPutStream)
import GHC.IO.Buffer (CharBuffer, isEmptyBuffer)
import GHC.IO.Exception (IOException(ioe_type), IOErrorType(InappropriateType))
import GHC.IO.Handle.Internals (augmentIOError, hClose_help, wantReadableHandle)
import GHC.IO.Handle.Types (BufferMode(..), Handle__(..), HandleType(..))
import System.IO (hGetBuffering, hFileSize, hSetBuffering, hTell)
import System.IO.Error (isEOFError)

-- | The 'readFile' function reads a file and returns the contents of
-- the file as a string.  The entire file is read strictly, as with
-- 'getContents'.
--
-- Beware that this function (similarly to 'Prelude.readFile') is locale-dependent.
-- Unexpected system locale may cause your application to read corrupted data or
-- throw runtime exceptions about "invalid argument (invalid byte sequence)"
-- or "invalid argument (invalid character)". This is also slow, because GHC
-- first converts an entire input to UTF-32, which is afterwards converted to UTF-8.
--
-- If your data is UTF-8,
-- using 'Data.Text.Encoding.decodeUtf8' '.' 'Data.ByteString.readFile'
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

catchError :: String -> Handle -> Handle__ -> IOError -> IO (Text, Bool)
catchError caller h Handle__{..} err
    | isEOFError err = do
        buf <- readIORef haCharBuffer
        return $ if isEmptyBuffer buf
                 then (T.empty, True)
                 else (T.singleton '\r', True)
    | otherwise = E.throwIO (augmentIOError err caller h)

-- | Wrap readChunk and return a value indicating if we're reached the EOF.
-- This is needed because unpack_nl is unable to discern the difference
-- between a buffer with just \r due to EOF or because not enough data was left
-- for decoding. e.g. the final character decoded from the byte buffer was \r.
readChunkEof :: Handle__ -> CharBuffer -> IO (Text, Bool)
readChunkEof hh buf = do t <- readChunk hh buf
                         return (t, False)

-- | /Experimental./ Read a single chunk of strict text from a
-- 'Handle'. The size of the chunk depends on the amount of input
-- currently buffered.
--
-- This function blocks only if there is no data available, and EOF
-- has not yet been reached. Once EOF is reached, this function
-- returns an empty string instead of throwing an exception.
hGetChunk :: Handle -> IO Text
hGetChunk h = wantReadableHandle "hGetChunk" h readSingleChunk
 where
  readSingleChunk hh@Handle__{..} = do
    buf <- readIORef haCharBuffer
    (t, _) <- readChunkEof hh buf `E.catch` catchError "hGetChunk" h hh
    return (hh, t)

-- | Read the remaining contents of a 'Handle' as a string.  The
-- 'Handle' is closed once the contents have been read, or if an
-- exception is thrown.
--
-- Internally, this function reads a chunk at a time from the
-- lower-level buffering abstraction, and concatenates the chunks into
-- a single string once the entire file has been read.
--
-- As a result, it requires approximately twice as much memory as its
-- result to construct its result.  For files more than a half of
-- available RAM in size, this may result in memory exhaustion.
hGetContents :: Handle -> IO Text
hGetContents h = do
  chooseGoodBuffering h
  wantReadableHandle "hGetContents" h readAll
 where
  readAll hh@Handle__{..} = do
    let readChunks = do
          buf <- readIORef haCharBuffer
          (t, eof) <- readChunkEof hh buf
                         `E.catch` catchError "hGetContents" h hh
          if eof
            then return [t]
            else (t:) `fmap` readChunks
    ts <- readChunks
    (hh', _) <- hClose_help hh
    return (hh'{haType=ClosedHandle}, T.concat ts)

-- | Use a more efficient buffer size if we're reading in
-- block-buffered mode with the default buffer size.  When we can
-- determine the size of the handle we're reading, set the buffer size
-- to that, so that we can read the entire file in one chunk.
-- Otherwise, use a buffer size of at least 16KB.
chooseGoodBuffering :: Handle -> IO ()
chooseGoodBuffering h = do
  bufMode <- hGetBuffering h
  case bufMode of
    BlockBuffering Nothing -> do
      d <- E.catch (liftM2 (-) (hFileSize h) (hTell h)) $ \(e::IOException) ->
           if ioe_type e == InappropriateType
           then return 16384 -- faster than the 2KB default
           else E.throwIO e
      when (d > 0) . hSetBuffering h . BlockBuffering . Just . fromInteger $ d
    _ -> return ()

-- | Read a single line from a handle.
hGetLine :: Handle -> IO Text
hGetLine = hGetLineWith T.concat

-- | Write a string to a handle.
hPutStr :: Handle -> Text -> IO ()
hPutStr h = hPutStream h . stream

-- | Write a string to a handle, followed by a newline.
hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn h = hPutStream h . streamLn

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
