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
import Tests.QuickCheckUtils (BigInt(Big))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Read as TL
import qualified Data.Text.Read as T

-- Reading.

t_decimal n s =
    T.signed T.decimal (T.pack (show n) `T.append` t) === Right (n,t)
    where t = T.dropWhile isDigit s
tl_decimal n s =
    TL.signed TL.decimal (TL.pack (show n) `TL.append` t) === Right (n,t)
    where t = TL.dropWhile isDigit s

t_decimal_int (n :: Int) = t_decimal n
t_decimal_integer (Big n) = t_decimal n

tl_decimal_int (n :: Int) = tl_decimal n
tl_decimal_integer (Big n) = tl_decimal n

t_hexadecimal n s ox =
    T.hexadecimal (T.concat [p, T.pack (showHex n ""), t]) === Right (n,t)
    where t = T.dropWhile isHexDigit s
          p = if ox then "0x" else ""
tl_hexadecimal n s ox =
    TL.hexadecimal (TL.concat [p, TL.pack (showHex n ""), t]) === Right (n,t)
    where t = TL.dropWhile isHexDigit s
          p = if ox then "0x" else ""

t_hexadecimal_int (Positive (n :: Int)) = t_hexadecimal n
t_hexadecimal_integer (Positive (Big n)) = t_hexadecimal n

tl_hexadecimal_int (Positive (n :: Int)) = tl_hexadecimal n
tl_hexadecimal_integer (Positive (Big n)) = tl_hexadecimal n

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

-- | Numbers without a fractional part, such as @123e-310@, which 'show' never produces.
t_read_rational_exponent :: Integer -> Int -> Property
t_read_rational_exponent m e =
  T.rational (T.pack s) === Right (read s :: Double, mempty)
  where s = show m ++ "e" ++ show (e `mod` 800 - 400)

tl_read_rational_exponent :: Integer -> Int -> Property
tl_read_rational_exponent m e =
  TL.rational (TL.pack s) === Right (read s :: Double, mempty)
  where s = show m ++ "e" ++ show (e `mod` 800 - 400)

-- | Numbers with the largest and smallest exponents in the range of 'Int'.
atIntBounds :: [String]
atIntBounds = ["1e" ++ show (maxBound :: Int), "1e" ++ show (minBound :: Int)]

-- | Numbers with exponents just outside the range of 'Int', and far outside it.
outOfIntRange :: [String]
outOfIntRange =
  [ "1e" ++ show (maxInt + 1), "1e" ++ show (negate maxInt - 2)
  , "1e" ++ show (maxInt + 1) ++ "0", "1e-" ++ show (maxInt + 1) ++ "0"
  , "1e18446744073709551616" ]
  where maxInt = toInteger (maxBound :: Int)

testRead :: TestTree
testRead =
  testGroup "read" [
    testProperty "t_decimal_int" t_decimal_int,
    testProperty "tl_decimal_int" tl_decimal_int,
    testProperty "t_hexadecimal_int" t_hexadecimal_int,
    testProperty "tl_hexadecimal_int" tl_hexadecimal_int,

    testProperty "t_decimal_integer" t_decimal_integer,
    testProperty "tl_decimal_integer" tl_decimal_integer,
    testProperty "t_hexadecimal_integer" t_hexadecimal_integer,
    testProperty "tl_hexadecimal_integer" tl_hexadecimal_integer,

    testProperty "t_double" $ t_read_double 1e-13,
    testProperty "tl_double" $ tl_read_double 1e-13,

    testProperty "t_rational" t_read_rational,
    testProperty "t_rational 1.3e-2" (t_read_rational 1.3e-2),
    testProperty "tl_rational" tl_read_rational,
    testProperty "tl_rational 9e-3" (tl_read_rational 9e-3),
    testProperty "t_rational_exponent" t_read_rational_exponent,
    testProperty "tl_rational_exponent" tl_read_rational_exponent,

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
        TL.rational (TL.pack "1.1e-1000000000") === Right (0.0 :: Double, mempty),

      testProperty "t_double exponent at the Int bounds" $
        map (T.double . T.pack) atIntBounds === [Right (1 / 0, mempty), Right (0.0, mempty)],
      testProperty "tl_double exponent at the Int bounds" $
        map (TL.double . TL.pack) atIntBounds === [Right (1 / 0, mempty), Right (0.0, mempty)],
      testProperty "t_rational exponent at the Int bounds" $
        map (T.rational . T.pack) atIntBounds === [Right (1 / 0 :: Double, mempty), Right (0.0, mempty)],
      testProperty "tl_rational exponent at the Int bounds" $
        map (TL.rational . TL.pack) atIntBounds === [Right (1 / 0 :: Double, mempty), Right (0.0, mempty)],

      testProperty "t_double exponent out of Int range" $
        conjoin [T.double (T.pack t) === Left "exponent out of range" | t <- outOfIntRange],
      testProperty "tl_double exponent out of Int range" $
        conjoin [TL.double (TL.pack t) === Left "exponent out of range" | t <- outOfIntRange],
      testProperty "t_rational exponent out of Int range" $
        conjoin [T.rational (T.pack t) === (Left "exponent out of range" :: Either String (Double, T.Text)) | t <- outOfIntRange],
      testProperty "tl_rational exponent out of Int range" $
        conjoin [TL.rational (TL.pack t) === (Left "exponent out of range" :: Either String (Double, TL.Text)) | t <- outOfIntRange],

      testProperty "t_double long exponent" $ once $
        T.double (T.append "1e" (T.replicate 100000 "9")) === Left "exponent out of range",
      testProperty "tl_double long exponent" $ once $
        TL.double (TL.append "1e-" (TL.replicate 100000 "9")) === Left "exponent out of range",
      testProperty "t_double exponent with many leading zeros" $ once $
        T.double (T.concat ["1e-", T.replicate 100000 "0", "3"]) === Right (1.0e-3, mempty),
      testProperty "tl_double exponent with many leading zeros" $ once $
        TL.double (TL.concat ["1e", TL.replicate 100000 "0", "3"]) === Right (1000.0, mempty)
      ]

    ]
