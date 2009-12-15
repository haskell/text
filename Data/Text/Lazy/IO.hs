{-# LANGUAGE BangPatterns, CPP, RecordWildCards #-}
-- |
-- Module      : Data.Text.Lazy.IO
-- Copyright   : (c) Bryan O'Sullivan 2009,
--               (c) Simon Marlow 2009
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Efficient locale-sensitive support for text I\/O.

module Data.Text.Lazy.IO
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

import Data.Text.Lazy (Text)
import Prelude hiding (appendFile, getContents, getLine, interact, putStr,
                       putStrLn, readFile, writeFile)
import System.IO (Handle, IOMode(..), hPutChar, openFile, stdin, stdout,
                  withFile)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
#if __GLASGOW_HASKELL__ <= 610
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
#else
import Control.Exception (throw)
import Data.IORef (readIORef)
import Data.Text.IO.Internal (hGetLineWith, readChunk)
import Data.Text.Lazy.Internal (chunk, empty)
import GHC.IO.Buffer (isEmptyBuffer)
import GHC.IO.Exception (IOException(..), IOErrorType(..), ioException)
import GHC.IO.Handle.Internals (augmentIOError, hClose_help,
                                wantReadableHandle, withHandle)
import GHC.IO.Handle.Types (Handle__(..), HandleType(..))
import System.IO.Error (isEOFError)
import System.IO.Unsafe (unsafeInterleaveIO)
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

hGetContents :: Handle -> IO Text
#if __GLASGOW_HASKELL__ <= 610
hGetContents = fmap decodeUtf8 . L8.hGetContents
#else
hGetContents h =
    wantReadableHandle "hGetContents" h $ \hh -> do
      ts <- lazyRead h
      return (hh{haType=SemiClosedHandle}, ts)

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
       return (hh, chunk t ts)) `catch` \e -> do
         (hh', _) <- hClose_help hh
         if isEOFError e
           then return $ if isEmptyBuffer buf
                         then (hh', empty)
                         else (hh', L.singleton '\r')
           else throw (augmentIOError e "hGetContents" h)
#endif

-- | Read a single line from a handle.
hGetLine :: Handle -> IO Text
#if __GLASGOW_HASKELL__ <= 610
hGetLine = fmap (decodeUtf8 . L8.fromChunks . (:[])) . S8.hGetLine
#else
hGetLine = hGetLineWith L.fromChunks
#endif

-- | Write a string to a handle.
hPutStr :: Handle -> Text -> IO ()
hPutStr h = mapM_ (T.hPutStr h) . L.toChunks

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
-- /Note/: The behaviour of functions in this module depends on the
-- version of GHC you are using.
--
-- Beginning with GHC 6.12, text I\/O is performed using the system or
-- handle's current locale and line ending conventions.
--
-- Under GHC 6.10 and earlier, the system I\/O libraries /do not
-- support/ locale-sensitive I\/O or line ending conversion.  On these
-- versions of GHC, functions in this library all use UTF-8.  What
-- does this mean in practice?
--
-- * All data that is read will be decoded as UTF-8.
--
-- * Before data is written, it is first encoded as UTF-8.
--
-- * On both reading and writing, the platform's native newline
--   conversion is performed.
--
-- If you must use a non-UTF-8 locale on an older version of GHC, you
-- will have to perform the transcoding yourself, e.g. as follows:
--
-- > import qualified Data.ByteString.Lazy as B
-- > import Data.Text.Lazy (Text)
-- > import Data.Text.Lazy.Encoding (encodeUtf16)
-- >
-- > putStr_Utf16LE :: Text -> IO ()
-- > putStr_Utf16LE t = B.putStr (encodeUtf16LE t)
