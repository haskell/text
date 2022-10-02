-- | Regression tests for specific bugs.
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Regressions
    (
      tests
    ) where

import Control.Exception (SomeException, handle)
import Data.Char (isLetter)
import GHC.Exts (Int(..), sizeofByteArray#)
import System.IO
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure)
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as E
import qualified Data.Text.Internal as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Unsafe as T
import qualified Test.Tasty as F
import qualified Test.Tasty.HUnit as F
import System.Directory (removeFile)

import Tests.Utils (withTempFile)

-- Reported by Michael Snoyman: UTF-8 encoding a large lazy bytestring
-- caused either a segfault or attempt to allocate a negative number
-- of bytes.
lazy_encode_crash :: IO ()
lazy_encode_crash = withTempFile $ \ _ h ->
   LB.hPut h . LE.encodeUtf8 . LT.pack . replicate 100000 $ 'a'

-- Reported by Pieter Laeremans: attempting to read an incorrectly
-- encoded file can result in a crash in the RTS (i.e. not merely an
-- exception).
hGetContents_crash :: IO ()
hGetContents_crash = do
  (path, h) <- openTempFile "." "crashy.txt"
  B.hPut h (B.pack [0x78, 0xc4 ,0x0a]) >> hClose h
  h' <- openFile path ReadMode
  hSetEncoding h' utf8
  handle (\(_::SomeException) -> return ()) $
    T.hGetContents h' >> assertFailure "T.hGetContents should crash"
  hClose h'
  removeFile path

-- Reported by Ian Lynagh: attempting to allocate a sufficiently large
-- string (via either Array.new or Text.replicate) could result in an
-- integer overflow.
replicate_crash :: IO ()
replicate_crash = handle (\(_::SomeException) -> return ()) $
                  T.replicate (2^power) "0123456789abcdef" `seq`
                  assertFailure "T.replicate should crash"
  where
    power | maxBound == (2147483647::Int) = 28
          | otherwise                     = 60 :: Int

-- Reported by John Millikin: a UTF-8 decode error handler could
-- return a bogus substitution character, which we would write without
-- checking.
utf8_decode_unsafe :: IO ()
utf8_decode_unsafe = do
  let t = TE.decodeUtf8With (\_ _ -> Just '\xdc00') "\x80"
  assertBool "broken error recovery shouldn't break us" (t == "\xfffd")

-- Reported by Eric Seidel: we mishandled mapping Chars that fit in a
-- single Word16 to Chars that require two.
mapAccumL_resize :: IO ()
mapAccumL_resize = do
  let f a _ = (a, '\65536')
      count = 5
      val   = T.mapAccumL f (0::Int) (T.replicate count "a")
  assertEqual "mapAccumL should correctly fill buffers for four-byte results"
             (0, T.replicate count "\65536") val
  assertEqual "mapAccumL should correctly size buffers for four-byte results"
             (count * 4) (T.lengthWord8 (snd val))

-- See GitHub #197
t197 :: IO ()
t197 =
    assertBool "length (filter (==',') \"0,00\") should be 1" (currencyParser "0,00")
  where
    currencyParser x = cond == 1
      where
        cond = length fltr
        fltr = filter (== ',') x

t221 :: IO ()
t221 =
    assertEqual "toLower of large input shouldn't crash"
                (T.toLower (T.replicate 200000 "0") `seq` ())
                ()

t227 :: IO ()
t227 =
    assertEqual "take (-3) shouldn't crash with overflow"
                (T.length $ T.filter isLetter $ T.take (-3) "Hello! How are you doing today?")
                0

t280_fromString :: IO ()
t280_fromString =
    assertEqual "TB.fromString performs replacement on invalid scalar values"
                (TB.toLazyText (TB.fromString "\xD800"))
                (LT.pack "\xFFFD")

t280_singleton :: IO ()
t280_singleton =
    assertEqual "TB.singleton performs replacement on invalid scalar values"
                (TB.toLazyText (TB.singleton '\xD800'))
                (LT.pack "\xFFFD")

-- See GitHub issue #301
-- This tests whether the "TEXT take . drop -> unfused" rule is applied to the
-- slice function. When the slice function is fused, a new array will be
-- constructed that is shorter than the original array. Without fusion the
-- array remains unmodified.
t301 :: IO ()
t301 = do
    assertEqual "The length of the array remains the same despite slicing"
                (I# (sizeofByteArray# originalArr))
                (I# (sizeofByteArray# newArr))

    assertEqual "The new array still contains the original value"
                (T.Text (TA.ByteArray newArr) originalOff originalLen)
                original
  where
    !original@(T.Text (TA.ByteArray originalArr) originalOff originalLen) = T.pack "1234567890"
    !(T.Text (TA.ByteArray newArr) _off _len) = T.take 1 $ T.drop 1 original

t330 :: IO ()
t330 = do
  let decodeL = LE.decodeUtf8With E.lenientDecode
  assertEqual "The lenient decoding of lazy bytestrings should not depend on how they are chunked"
    (decodeL (LB.fromChunks [B.pack [194], B.pack [97, 98, 99]]))
    (decodeL (LB.fromChunks [B.pack [194, 97, 98, 99]]))

tests :: F.TestTree
tests = F.testGroup "Regressions"
    [ F.testCase "hGetContents_crash" hGetContents_crash
    , F.testCase "lazy_encode_crash" lazy_encode_crash
    , F.testCase "mapAccumL_resize" mapAccumL_resize
    , F.testCase "replicate_crash" replicate_crash
    , F.testCase "utf8_decode_unsafe" utf8_decode_unsafe
    , F.testCase "t197" t197
    , F.testCase "t221" t221
    , F.testCase "t227" t227
    , F.testCase "t280/fromString" t280_fromString
    , F.testCase "t280/singleton" t280_singleton
    , F.testCase "t301" t301
    , F.testCase "t330" t330
    ]
