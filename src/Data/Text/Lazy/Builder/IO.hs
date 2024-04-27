{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Data.Text.Lazy.Builder.IO
  ( hPutBuilder
  , hPutBuilderUtf8
  ) where

import Control.Monad (join)
import Control.Monad.ST (runST)
import Data.Array.Byte (ByteArray(ByteArray))
import Data.Foldable (foldrM, for_, traverse_)
import Data.Functor (void)
import Data.IORef (readIORef, writeIORef)
import Data.Text (unpack)
import Data.Text.Array (new, newPinned)
import Data.Text.Internal (Text(Text))
import Data.Text.Internal.Builder (Builder, getTexts)
import Data.Text.Internal.Fusion (stream)
import Data.Text.Internal.Fusion.Types (Step(..), Stream(..))
import Data.Text.Internal.Lazy (smallChunkSize)
import GHC.Exts (byteArrayContents#)
import GHC.IO.Buffer (Buffer(..), BufferState(..), CharBuffer, RawCharBuffer, emptyBuffer, newCharBuffer, writeCharBuf)
import GHC.IO.Encoding (textEncodingName)
import GHC.IO.Handle.Internals (flushWriteBuffer, wantWritableHandle, flushWriteBuffer)
import GHC.IO.Handle.Text (commitBuffer')
import GHC.IO.Handle.Types (Handle, Handle__(..), BufferMode(NoBuffering,LineBuffering,BlockBuffering), BufferList(BufferListCons,BufferListNil), Newline(LF,CRLF))
import GHC.Ptr (Ptr(Ptr))
import System.IO (hPutBuf, hPutChar)

hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder h b = do
  (mode, nl, isUtf8, buf) <- wantWritableHandle "hPutStr" h $ \(Handle__ {..}) -> do
    let isUtf8 = maybe False (("UTF-8" ==) . textEncodingName) haCodec
    -- modified getSpareBuffer
    buf <- case haBufferMode of
      NoBuffering -> error "no buffer!"
      BlockBuffering _ | haOutputNL == LF && isUtf8 -> error "no buffer!"
      _ -> do
        bufs <- readIORef haBuffers
        buf  <- readIORef haCharBuffer
        case bufs of
          BufferListCons b rest -> do
            writeIORef haBuffers rest
            return $ emptyBuffer b (bufSize buf) WriteBuffer
          BufferListNil -> newCharBuffer (bufSize buf) WriteBuffer
    return (haBufferMode, haOutputNL, isUtf8, buf)
  case mode of
    NoBuffering      -> hPutChars h b
    LineBuffering    -> writeLines h nl buf b
    BlockBuffering _
      | nl == CRLF   -> writeBlocksCRLF h buf b
      | isUtf8     -> hPutBuilderUtf8 h b
      | otherwise    -> writeBlocksRaw h buf b

hPutBuilderUtf8 :: Handle -> Builder -> IO ()
hPutBuilderUtf8 h b = do
  flushBytes
    -- ????? Does the text ByteArray have a chance of being garbage collected before the flush finishes?
  for_ textBuffers $ \(Text (ByteArray a#) _ bufR) -> hPutBuf h (Ptr (byteArrayContents# a#)) bufR
  where
  flushBytes = wantWritableHandle "hPutBuilder" h flushWriteBuffer
  textBuffers = runST $ getTexts bufferSize newPinned b

bufferSize :: Int
bufferSize = 1024 -- I don't know what this should be

hPutChars :: Handle -> Builder -> IO ()
hPutChars h b
  = traverse_ (hPutChar h)
  . join
  . fmap unpack
  $ runST (getTexts bufferSize' new b)

writeLines :: Handle -> Newline -> CharBuffer -> Builder -> IO ()
writeLines h nl buf b = do
  (Buffer {bufRaw, bufSize}, n) <- foldrM
    (\t (buf, n) -> outer (stream t) buf n)
    (buf, 0)
    (runST $ getTexts bufferSize' new b)
  void $ commitBuffer h bufRaw bufSize n Flush Release
  where
  outer (Stream next s len) buf@(Buffer {bufRaw, bufSize}) !n = inner s n
    where
    inner !s !n = case next s of
      Done -> return (buf, n)
      Skip s -> inner s n
      Yield c s
        | n + 1 >= bufSize -> do
          buf <- commitBuffer h bufRaw bufSize n Flush NoRelease
          outer (Stream next s len) buf 0
        | c == '\n' -> do
          n <- if nl == CRLF
            then do
              n <- writeCharBuf bufRaw n '\r'
              writeCharBuf bufRaw n '\n'
            else writeCharBuf bufRaw n '\n'
          buf <- commitBuffer h bufRaw bufSize n Flush NoRelease
          outer (Stream next s len) buf 0
        | otherwise -> writeCharBuf bufRaw n c >>= inner s

writeBlocksCRLF :: Handle -> CharBuffer -> Builder -> IO ()
writeBlocksCRLF h buf b = do
  (Buffer {bufRaw, bufSize}, n) <- foldrM
    (\t (buf, n) -> outer (stream t) buf n)
    (buf, 0)
    (runST $ getTexts bufferSize' new b)
  void $ commitBuffer h bufRaw bufSize n Flush Release
  where
  outer (Stream next s len) buf@(Buffer {bufRaw, bufSize}) !n = inner s n
    where
    inner !s !n = case next s of
      Done -> return (buf, n)
      Skip s -> inner s n
      Yield c s
        | n + 1 >= bufSize -> do
          buf <- commitBuffer h bufRaw bufSize n Flush NoRelease
          outer (Stream next s len) buf 0
        | c == '\n' -> do
          n <- writeCharBuf bufRaw n '\r'
          n <- writeCharBuf bufRaw n '\n'
          inner s n
        | otherwise -> writeCharBuf bufRaw n c >>= inner s

writeBlocksRaw :: Handle -> CharBuffer -> Builder -> IO ()
writeBlocksRaw h buf b = do
  (Buffer {bufRaw, bufSize}, n) <- foldrM
    (\t (buf, n) -> outer (stream t) buf n)
    (buf, 0)
    (runST $ getTexts bufferSize' new b)
  void $ commitBuffer h bufRaw bufSize n Flush Release
  where
  outer (Stream next s len) buf@(Buffer {bufRaw, bufSize}) !n = inner s n
    where
    inner !s !n = case next s of
      Done -> return (buf, n)
      Skip s -> inner s n
      Yield c s
        | n >= bufSize -> do
          buf <- commitBuffer h bufRaw bufSize n Flush NoRelease
          outer (Stream next s len) buf 0
        | otherwise -> writeCharBuf bufRaw n c >>= inner s

bufferSize' :: Int
bufferSize' = smallChunkSize


-- This function is completely lifted from GHC.IO.Handle.Text.
commitBuffer :: Handle -> RawCharBuffer -> Int -> Int -> Flush -> Release
             -> IO CharBuffer
commitBuffer hdl !raw !sz !count flush release =
  wantWritableHandle "commitAndReleaseBuffer" hdl $
     commitBuffer' raw sz count (flushBool flush) (releaseBool release)
{-# INLINE commitBuffer #-}

data Flush = Flush | NoFlush
flushBool :: Flush -> Bool
flushBool Flush = True
flushBool NoFlush = False

data Release = Release | NoRelease
releaseBool :: Release -> Bool
releaseBool Release = True
releaseBool NoRelease = False
