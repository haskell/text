{-# LANGUAGE BangPatterns, NamedFieldPuns, OverloadedStrings, UnboxedTuples, CPP #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      : Data.Text.Read
-- Copyright   : (c) 2010, 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- Functions used frequently when reading textual data.
module Data.Text.Read
    (
      Reader
    , decimal
    , hexadecimal
    , signed
    , rational
    , double
    ) where

import Control.Monad (liftM)
import Data.Char (ord)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Ratio ((%))
import Data.Text as T
import Data.Text.Internal as T (Text(..))
import Data.Text.Array as A
import Data.Text.Internal.Private (spanAscii_)
import Data.Text.Internal.Read
import Data.Text.Internal.Builder.Int.Digits (Base(..), base10, base16)
import qualified Data.Text.Unsafe as TU
import Data.Word (Word, Word8, Word16, Word32, Word64)

-- | Read some text.  If the read succeeds, return its value and the
-- remaining text, otherwise an error message.
type Reader a = Text -> Either String (a, Text)
type Parser a = IParser Text a

-- | Read a decimal integer.  The input must begin with at least one
-- decimal digit, and is consumed until a non-digit or end of string
-- is reached.
--
-- This function does not handle leading sign characters.  If you need
-- to handle signed input, use @'signed' 'decimal'@.
--
-- /Note/: For fixed-width integer types, this function does not
-- attempt to detect overflow, so a sufficiently long input may give
-- incorrect results.  If you are worried about overflow, use
-- 'Integer' for your result type.
decimal :: Integral a => Reader a
{-# SPECIALIZE decimal :: Reader Int #-}
{-# SPECIALIZE decimal :: Reader Int8 #-}
{-# SPECIALIZE decimal :: Reader Int16 #-}
{-# SPECIALIZE decimal :: Reader Int32 #-}
{-# SPECIALIZE decimal :: Reader Int64 #-}
{-# SPECIALIZE decimal :: Reader Integer #-}
{-# SPECIALIZE decimal :: Reader Data.Word.Word #-}
{-# SPECIALIZE decimal :: Reader Word8 #-}
{-# SPECIALIZE decimal :: Reader Word16 #-}
{-# SPECIALIZE decimal :: Reader Word32 #-}
{-# SPECIALIZE decimal :: Reader Word64 #-}
decimal txt
    | T.null h  = Left "input does not start with a digit"
    | otherwise = Right (integer base10 (fromIntegral . digitToInt) h, t)
  where (# h,t #)  = spanAscii_ (\w -> w - ord8 '0' < 10) txt

-- | Read a hexadecimal integer, consisting of an optional leading
-- @\"0x\"@ followed by at least one hexadecimal digit. Input is
-- consumed until a non-hex-digit or end of string is reached.
-- This function is case insensitive.
--
-- This function does not handle leading sign characters.  If you need
-- to handle signed input, use @'signed' 'hexadecimal'@.
--
-- /Note/: For fixed-width integer types, this function does not
-- attempt to detect overflow, so a sufficiently long input may give
-- incorrect results.  If you are worried about overflow, use
-- 'Integer' for your result type.
hexadecimal :: Integral a => Reader a
{-# SPECIALIZE hexadecimal :: Reader Int #-}
{-# SPECIALIZE hexadecimal :: Reader Int8 #-}
{-# SPECIALIZE hexadecimal :: Reader Int16 #-}
{-# SPECIALIZE hexadecimal :: Reader Int32 #-}
{-# SPECIALIZE hexadecimal :: Reader Int64 #-}
{-# SPECIALIZE hexadecimal :: Reader Integer #-}
{-# SPECIALIZE hexadecimal :: Reader Word #-}
{-# SPECIALIZE hexadecimal :: Reader Word8 #-}
{-# SPECIALIZE hexadecimal :: Reader Word16 #-}
{-# SPECIALIZE hexadecimal :: Reader Word32 #-}
{-# SPECIALIZE hexadecimal :: Reader Word64 #-}
hexadecimal txt
    | h == "0x" || h == "0X" = hex t
    | otherwise              = hex txt
 where (h,t) = T.splitAt 2 txt

hex :: Integral a => Reader a
{-# SPECIALIZE hex :: Reader Int #-}
{-# SPECIALIZE hex :: Reader Int8 #-}
{-# SPECIALIZE hex :: Reader Int16 #-}
{-# SPECIALIZE hex :: Reader Int32 #-}
{-# SPECIALIZE hex :: Reader Int64 #-}
{-# SPECIALIZE hex :: Reader Integer #-}
{-# SPECIALIZE hex :: Reader Word #-}
{-# SPECIALIZE hex :: Reader Word8 #-}
{-# SPECIALIZE hex :: Reader Word16 #-}
{-# SPECIALIZE hex :: Reader Word32 #-}
{-# SPECIALIZE hex :: Reader Word64 #-}
hex txt
    | T.null h  = Left "input does not start with a hexadecimal digit"
    | otherwise = Right (integer base16 (fromIntegral . hexDigitToInt) h, t)
  where (# h,t #)  = spanAscii_ (\w -> w - ord8 '0' < 10 || w - ord8 'A' < 6 || w - ord8 'a' < 6) txt

-- | Internal function to read a base-@base@ number faster than the naive quadratic algorithm.
--
-- The input text must only contain digits. Overflows are allowed.
integer :: Integral a => Base -> (Char -> Word) -> Text -> a
integer (Base { base, maxWord, maxDigits }) digit t0
  | TU.lengthWord8 t0 <= maxDigits = fromIntegral (word base digit t0)
  | otherwise = scanWords [word' (TU.takeWord8 mswSize t0)] (TU.dropWord8 mswSize t0)
  where
    -- Split the most significant word from t0 so the remainder's length is a multiple of maxDigits
    mswSize = (TU.lengthWord8 t0 - 1) `mod` maxDigits + 1
    word' = fromIntegral . word base digit

    -- The length of t is a positive multiple of maxDigits
    scanWords chunks t
      | TU.lengthWord8 t <= maxDigits = reduce (fromIntegral maxWord) (word' t : chunks)
      | otherwise = scanWords (word' (TU.takeWord8 maxDigits t) : chunks) (TU.dropWord8 maxDigits t)

    reduce !_ [c] = c
    reduce width cs = reduce (width * width) (pairs width cs)

    pairs !_ [] = []
    pairs _ [c] = [c]
    pairs width (c0 : c1 : cs) = c0 + width * c1 : pairs width cs

-- | Internal function to read a base-@b@ number into a 'Word'.
--
-- The text must only contain digits and the result must not to overflow 'Word'.
word :: Word -> (Char -> Word) -> Text -> Word
word base digit = T.foldl' (\n d -> n * base + fromIntegral (digit d)) 0

-- | Read an optional leading sign character (@\'-\'@ or @\'+\'@) and
-- apply it to the result of applying the given reader.
signed :: Num a => Reader a -> Reader a
{-# INLINE signed #-}
signed f = runP (signa (P f))

-- | Read a rational number.
--
-- This function accepts an optional leading sign character, followed
-- by at least one decimal digit.  The syntax similar to that accepted
-- by the 'read' function, with the exception that a trailing @\'.\'@
-- or @\'e\'@ /not/ followed by a number is not consumed.
--
-- Examples (with behaviour identical to 'read'):
--
-- >rational "3"     == Right (3.0, "")
-- >rational "3.1"   == Right (3.1, "")
-- >rational "3e4"   == Right (30000.0, "")
-- >rational "3.1e4" == Right (31000.0, "")
-- >rational ".3"    == Left "input does not start with a digit"
-- >rational "e3"    == Left "input does not start with a digit"
--
-- Examples of differences from 'read':
--
-- >rational "3.foo" == Right (3.0, ".foo")
-- >rational "3e"    == Right (3.0, "e")
rational :: Fractional a => Reader a
{-# SPECIALIZE rational :: Reader Double #-}
rational = floaty $ \real frac fracDenom power ->
  -- We must be careful to prevent DDoS attacks: if the return type is 'Double',
  -- a client rightfully expects 'rational' to operate within bounded memory.
  -- Thus if power is small, we can compute fraction with full precision and divide.
  -- Otherwise divide first, apply fromRational and scale last:
  -- the small loss of precision for Double does not matter much because the result is
  -- likely infinity or zero anyway.
  if abs power < 1000
  then fromRational ((real % 1 + frac % fracDenom) * (10 ^^ power))
  else fromRational (real % 1 + frac % fracDenom) * (10 ^^ power)

-- | Read a rational number.
--
-- The syntax accepted by this function is the same as for 'rational'.
--
-- /Note/: This function is almost ten times faster than 'rational',
-- but is slightly less accurate.
--
-- The 'Double' type supports about 16 decimal places of accuracy.
-- For 94.2% of numbers, this function and 'rational' give identical
-- results, but for the remaining 5.8%, this function loses precision
-- around the 15th decimal place.  For 0.001% of numbers, this
-- function will lose precision at the 13th or 14th decimal place.
double :: Reader Double
double = floaty $ \real frac fracDenom power ->
                   (fromInteger real +
                   fromInteger frac / fromInteger fracDenom) * (10 ^^ power)

signa :: Num a => Parser a -> Parser a
{-# SPECIALIZE signa :: Parser Int -> Parser Int #-}
{-# SPECIALIZE signa :: Parser Int8 -> Parser Int8 #-}
{-# SPECIALIZE signa :: Parser Int16 -> Parser Int16 #-}
{-# SPECIALIZE signa :: Parser Int32 -> Parser Int32 #-}
{-# SPECIALIZE signa :: Parser Int64 -> Parser Int64 #-}
{-# SPECIALIZE signa :: Parser Integer -> Parser Integer #-}
signa p = do
  sign <- perhaps (ord8 '+') $ charAscii (\c -> c == ord8 '-' || c == ord8 '+')
  if sign == ord8 '+' then p else negate `liftM` p

charAscii :: (Word8 -> Bool) -> Parser Word8
charAscii p = P $ \(Text arr off len) -> let c = A.unsafeIndex arr off in
  if len > 0 && p c
  then Right (c, Text arr (off + 1) (len - 1))
  else Left "character does not match"

floaty :: Fractional a => (Integer -> Integer -> Integer -> Int -> a) -> Reader a
{-# INLINE floaty #-}
floaty f = runP $ do
  sign <- perhaps (ord8 '+') $ charAscii (\c -> c == ord8 '-' || c == ord8 '+')
  real <- P decimal
  T fraction fracDigits <- perhaps (T 0 0) $ do
    _ <- charAscii (== ord8 '.')
    digits <- P $ \t -> Right (let (# hd, _ #) = spanAscii_ (\w -> w - ord8 '0' < 10) t in T.length hd, t)
    n <- P decimal
    return $ T n digits
  let e c = c == ord8 'e' || c == ord8 'E'
  power <- perhaps 0 (charAscii e >> signa (P decimal) :: Parser Int)
  let n = if fracDigits == 0
          then if power == 0
               then fromInteger real
               else fromInteger real * (10 ^^ power)
          else f real fraction (10 ^ fracDigits) power
  return $! if sign == ord8 '+'
            then n
            else -n

ord8 :: Char -> Word8
ord8 = fromIntegral . ord
