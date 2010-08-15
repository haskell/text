{-# LANGUAGE BangPatterns #-}

import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as S

-- Text
foldLinesT :: (a -> T.Text -> a) -> a -> Handle -> IO a
foldLinesT f z0 h = go z0
  where
    go !z = do
        eof <- hIsEOF h
        if eof
            then return z
            else do
                l <- T.hGetLine h
                let z' = f z l in go z'
{-# INLINE foldLinesT #-}

testT :: Handle -> IO Int
testT = foldLinesT (\n _ -> n + 1) 0

--ByteString
foldLinesB :: (a -> S.ByteString -> a) -> a -> Handle -> IO a
foldLinesB f z0 h = go z0
  where
    go !z = do
        eof <- hIsEOF h
        if eof
            then return z
            else do
                l <- S.hGetLine h
                let z' = f z l in go z'
{-# INLINE foldLinesB #-}

testB :: Handle -> IO Int
testB = foldLinesB (\n _ -> n + 1) 0

main = do
  (name : file : _) <- getArgs
  h <- openFile file ReadMode
  hSetBuffering h (BlockBuffering (Just 16384))
  case name of
    "bs" -> testB h
    "text" -> testT h
