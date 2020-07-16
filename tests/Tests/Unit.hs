-- | Tests for specific cases.
--
{-# LANGUAGE OverloadedStrings #-}
module Tests.Unit
    (
      tests
    ) where

import Data.Int (Int8)
import Test.HUnit ((@?=))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as Int
import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F

paddedDecimalTests :: F.Test
paddedDecimalTests = F.testGroup "paddedDecimal"
  [ tI 3 12 "012"
  , tI 3 1234 "1234"
  , tI 3 (-123) "-123"
  , tI 3 (-12) "-012"
  , tI 3 0 "000"
  , tI 0 0 "0"
  , tI 3 10 "010"
  , tI 3 (-10) "-010"
  , tI 3 (-1) "-001"
  , tI 7 1234 "0001234"
  , tI (-3) 12 "12"
  , tI 1 (-3) "-3"
  , tI8 5 (-128) "-00128"
  , tI8 3 (-128) "-128"
  , tI8 2 (-128) "-128"
  ]
  where
  tI :: Int -> Int -> TL.Text -> F.Test
  tI padLen input expected = F.testCase ("Int " ++ show (padLen, input)) $
    TB.toLazyText (Int.paddedDecimal padLen input) @?= expected

  tI8 :: Int -> Int8 -> TL.Text -> F.Test
  tI8 padLen input expected = F.testCase ("Int8 " ++ show (padLen, input)) $
    TB.toLazyText (Int.paddedDecimal padLen input) @?= expected

tests :: F.Test
tests = F.testGroup "unit tests"
  [ paddedDecimalTests
  ]
