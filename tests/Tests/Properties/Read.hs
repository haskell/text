-- | Tests for readers

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tests.Properties.Read
    ( testRead
    ) where

import Data.Char (isDigit, isHexDigit)
import Numeric (showHex)
import Test.Tasty (TestTree, testGroup, localOption, mkTimeout)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Tests.QuickCheckUtils ()
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Read as TL
import qualified Data.Text.Read as T

-- Reading.

t_decimal (n::Int) s =
    T.signed T.decimal (T.pack (show n) `T.append` t) === Right (n,t)
    where t = T.dropWhile isDigit s
tl_decimal (n::Int) s =
    TL.signed TL.decimal (TL.pack (show n) `TL.append` t) === Right (n,t)
    where t = TL.dropWhile isDigit s
t_hexadecimal m s ox =
    T.hexadecimal (T.concat [p, T.pack (showHex n ""), t]) === Right (n,t)
    where t = T.dropWhile isHexDigit s
          p = if ox then "0x" else ""
          n = getPositive m :: Int
tl_hexadecimal m s ox =
    TL.hexadecimal (TL.concat [p, TL.pack (showHex n ""), t]) === Right (n,t)
    where t = TL.dropWhile isHexDigit s
          p = if ox then "0x" else ""
          n = getPositive m :: Int

isFloaty c = c `elem` ("+-.0123456789eE" :: String)

t_read_rational :: Double -> T.Text -> Property
t_read_rational n s =
  case T.rational (T.pack (show n) `T.append` t) of
    Left err       -> counterexample err $ property False
    Right (n', t') -> t === t' .&&. n' === n''
  where
    t = T.dropWhile isFloaty s
    n'' = read (show n) :: Double

t_read_double :: Double -> Double -> T.Text -> Property
t_read_double tol n s =
  case T.double (T.pack (show n) `T.append` t) of
    Left err       -> counterexample err $ property False
    Right (n', t') -> t === t' .&&. property (abs (n - n') <= tol)
  where
    t = T.dropWhile isFloaty s

tl_read_rational :: Double -> TL.Text -> Property
tl_read_rational n s =
  case TL.rational (TL.pack (show n) `TL.append` t) of
    Left err       -> counterexample err $ property False
    Right (n', t') -> t === t' .&&. n' === n''
  where
    t = TL.dropWhile isFloaty s
    n'' = read (show n) :: Double

tl_read_double :: Double -> Double -> TL.Text -> Property
tl_read_double tol n s =
  case TL.rational (TL.pack (show n) `TL.append` t) of
    Left err       -> counterexample err $ property False
    Right (n', t') -> t === t' .&&. property (abs (n - n') <= tol)
  where
    t = TL.dropWhile isFloaty s

testRead :: TestTree
testRead =
  testGroup "read" [
    testProperty "t_decimal" t_decimal,
    testProperty "tl_decimal" tl_decimal,
    testProperty "t_hexadecimal" t_hexadecimal,
    testProperty "tl_hexadecimal" tl_hexadecimal,

    testProperty "t_double" $ t_read_double 1e-13,
    testProperty "tl_double" $ tl_read_double 1e-13,

    testProperty "t_rational" t_read_rational,
    testProperty "t_rational 1.3e-2" (t_read_rational 1.3e-2),
    testProperty "tl_rational" tl_read_rational,
    testProperty "tl_rational 9e-3" (tl_read_rational 9e-3),

    localOption (mkTimeout 100000) $ testGroup "DDoS attacks" [
      testProperty "t_double large positive exponent" $
        T.double (T.pack "1.1e1000000000") === Right (1 / 0, mempty),
      testProperty "t_double large negative exponent" $
        T.double (T.pack "1.1e-1000000000") === Right (0.0, mempty),
      testProperty "tl_double large positive exponent" $
        TL.double (TL.pack "1.1e1000000000") === Right (1 / 0, mempty),
      testProperty "tl_double large negative exponent" $
        TL.double (TL.pack "1.1e-1000000000") === Right (0.0, mempty),

      testProperty "t_rational large positive exponent" $
        T.rational (T.pack "1.1e1000000000") === Right (1 / 0 :: Double, mempty),
      testProperty "t_rational large negative exponent" $
        T.rational (T.pack "1.1e-1000000000") === Right (0.0 :: Double, mempty),
      testProperty "tl_rational large positive exponent" $
        TL.rational (TL.pack "1.1e1000000000") === Right (1 / 0 :: Double, mempty),
      testProperty "tl_rational large negative exponent" $
        TL.rational (TL.pack "1.1e-1000000000") === Right (0.0 :: Double, mempty)
      ]

    ]
