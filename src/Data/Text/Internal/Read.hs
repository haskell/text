-- |
-- Module      : Data.Text.Internal.Read
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Common internal functions for reading textual data.
module Data.Text.Internal.Read
    (
      IReader
    , IParser(..)
    , T(..)
    , digitToInt
    , hexDigitToInt
    , perhaps
    ) where

import Control.Applicative as App (Applicative(..))
import Control.Arrow (first)
import Control.Monad (ap)
import Data.Char (ord)

type IReader t a = t -> Either String (a,t)

newtype IParser t a = P {
      runP :: IReader t a
    }

instance Functor (IParser t) where
    fmap f m = P $ fmap (first f) . runP m

instance Applicative (IParser t) where
    pure a = P $ \t -> Right (a,t)
    {-# INLINE pure #-}
    (<*>) = ap

instance Monad (IParser t) where
    return = App.pure
    m >>= k  = P $ \t -> case runP m t of
                           Left err     -> Left err
                           Right (a,t') -> runP (k a) t'
    {-# INLINE (>>=) #-}

-- If we ever need a `MonadFail` instance the definition below can be used
--
-- > instance MonadFail (IParser t) where
-- >   fail msg = P $ \_ -> Left msg
--
-- But given the code compiles fine with a post-MFP GHC 8.6+ we don't need
-- one just yet.

data T = T !Integer !Int

perhaps :: a -> IParser t a -> IParser t a
perhaps def m = P $ \t -> case runP m t of
                            Left _      -> Right (def,t)
                            r@(Right _) -> r

hexDigitToInt :: Char -> Int
hexDigitToInt c
    | to0 < 10  = wordToInt to0
    | toa < 6   = wordToInt toa + 10
    | otherwise = wordToInt toA + 10
    where
        ordW = intToWord (ord c)
        to0 = ordW - intToWord (ord '0')
        toa = ordW - intToWord (ord 'a')
        toA = ordW - intToWord (ord 'A')

digitToInt :: Char -> Int
digitToInt c = ord c - ord '0'

intToWord :: Int -> Word
intToWord = fromIntegral

wordToInt :: Word -> Int
wordToInt = fromIntegral
