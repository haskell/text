{-# LANGUAGE ScopedTypeVariables #-}

-- Regression tests for specific bugs.

import Control.Exception (SomeException, bracket, handle)
import Control.Monad (when)
import System.Directory (removeFile)
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE
import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F
import Test.HUnit (assertFailure)

withTempFile = bracket (openTempFile "." "crashy.txt") cleanupTemp . uncurry
  where
    cleanupTemp (path,h) = do
      open <- hIsOpen h
      when open (hClose h)
      removeFile path

-- Reported by Michael Snoyman: UTF-8 encoding a large lazy bytestring
-- caused either a segfault or attempt to allocate a negative number
-- of bytes.
lazy_encode_crash = withTempFile $ \ _ h ->
   LB.hPut h . LE.encodeUtf8 . LT.pack . replicate 100000 $ 'a'

-- Reported by Pieter Laeremans: attempting to read an incorrectly
-- encoded file can result in a crash in the RTS (i.e. not merely an
-- exception).
hGetContents_crash = withTempFile $ \ path h -> do
  B.hPut h (B.pack [0x78, 0xc4 ,0x0a]) >> hClose h
  h' <- openFile path ReadMode
  hSetEncoding h' utf8
  handle (\(_::SomeException) -> return ()) $
    T.hGetContents h' >> assertFailure "T.hGetContents should crash"

tests :: F.Test
tests = F.testGroup "crashers" [
          F.testCase "hGetContents_crash" hGetContents_crash
        , F.testCase "lazy_encode_crash" lazy_encode_crash
        ]

main = F.defaultMain [tests]
