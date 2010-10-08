{-# LANGUAGE OverloadedStrings, Rank2Types #-}

-- |
-- Module      : Data.Text.Lex
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- Lexing functions used frequently when handling textual data.
module Data.Text.Lex
    (
      Lexer
    , decimal
    , hexadecimal
    , signed
    , rational
    ) where

import Control.Monad (liftM)
import Data.Char (digitToInt, isDigit, isHexDigit, ord)
import Data.Ratio
import Data.Text as T

-- | Read some text, and if the read succeeds, return its value and
-- the remaining text.
type Lexer a = Text -> Either String (a,Text)

-- | Read a decimal number.
decimal :: Integral a => Lexer a
{-# SPECIALIZE decimal :: Lexer Int #-}
{-# SPECIALIZE decimal :: Lexer Integer #-}
decimal txt
    | T.null h  = Left "no digits in input"
    | otherwise = Right (T.foldl' go 0 h, t)
  where (h,t)  = T.spanBy isDigit txt
        go n d = (n * 10 + fromIntegral (digitToInt d))

-- | Read a hexadecimal number, with optional leading @\"0x\"@.  This
-- function is case insensitive.
hexadecimal :: Integral a => Lexer a
{-# SPECIALIZE hex :: Lexer Int #-}
{-# SPECIALIZE hex :: Lexer Integer #-}
hexadecimal txt
    | T.toLower h == "0x" = hex t
    | otherwise           = hex txt
 where (h,t) = T.splitAt 2 txt

hex :: Integral a => Lexer a
{-# SPECIALIZE hex :: Lexer Int #-}
{-# SPECIALIZE hex :: Lexer Integer #-}
hex txt
    | T.null h  = Left "no digits in input"
    | otherwise = Right (T.foldl' go 0 h, t)
  where (h,t)  = T.spanBy isHexDigit txt
        go n d = (n * 16 + fromIntegral (hexDigitToInt d))

hexDigitToInt :: Char -> Int
hexDigitToInt c
    | c >= '0' && c <= '9' = ord c - ord '0'
    | c >= 'a' && c <= 'f' = ord c - (ord 'a' - 10)
    | c >= 'A' && c <= 'F' = ord c - (ord 'A' - 10)
    | otherwise            = error "Data.Text.Lex.hexDigitToInt: bad input"

-- | Read a leading sign character (@\'-\'@ or @\'+\'@) and apply it
-- to the result of applying the given reader.
signed :: Num a => Lexer a -> Lexer a
{-# INLINE signed #-}
signed f = parse (signa (asP f))

signa :: Num a => Parser a -> Parser a
{-# SPECIALIZE signa :: Parser Int -> Parser Int #-}
{-# SPECIALIZE signa :: Parser Integer -> Parser Integer #-}
signa p = do
  sign <- perhaps '+' $ char (`elem` "-+")
  if sign == '+' then p else negate `liftM` p

newtype Parser a = P {
      runP :: forall r.
              (String -> Either String (r,Text))
           -> (a -> Text -> Either String (r,Text))
           -> Text -> Either String (r,Text)
    }

parse :: Parser a -> Text -> Either String (a,Text)
parse p t = runP p Left (\a t' -> Right (a,t')) t

instance Monad Parser where
    return a = P $ \kf ks t -> ks a t
    {-# INLINE return #-}
    m >>= k  = P $ \kf ks t -> runP m kf (\a t' -> runP (k a) kf ks t') t
    {-# INLINE (>>=) #-}
    fail msg = P $ \kf ks t -> kf msg

perhaps :: a -> Parser a -> Parser a
perhaps def m = P $ \kf ks t -> runP m (\_ -> ks def t) ks t

char :: (Char -> Bool) -> Parser Char
char p = P $ \kf ks t -> case T.uncons t of
                           Just (c,t') | p c -> ks c t'
                           _                 -> kf "char"

asP :: (Text -> Either String (a,Text)) -> Parser a
asP p = P $ \kf ks t -> case p t of
                          Left err -> kf err
                          Right (a,t') -> ks a t'

-- | Read a rational number.
rational :: RealFloat a => Lexer a
{-# SPECIALIZE rational :: Lexer Double #-}
rational = parse $ do
  real <- signa (asP decimal)
  (fraction,fracDigits) <- perhaps (0,0) $ do
    _ <- char (=='.')
    digits <- P $ \kf ks t -> ks (T.length $ T.takeWhile isDigit t) t
    n <- asP decimal
    return (n, digits)
  power <- perhaps 0 (char (`elem` "eE") >> signa (asP decimal) :: Parser Int)
  return $! if fraction == 0
            then if power == 0
                 then fromIntegral real
                 else fromIntegral real * (10 ^^ power)
            else fromRational $ if power == 0
                 then real % 1 + fraction % (10 ^ fracDigits)
                 else (real % 1 + fraction % (10 ^ fracDigits)) * (10 ^^ power)
