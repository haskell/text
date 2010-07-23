{-# LANGUAGE FlexibleInstances #-}

module QuickCheckUtils where

import Control.Arrow (first)
import Data.Int (Int64)
import Data.Word (Word8, Word16, Word32)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Random (Random(..), RandomGen)
import Test.QuickCheck (Arbitrary(..), choose, sized, vector)
import qualified Data.ByteString as B

instance Random Int64 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary Int64 where
    arbitrary     = choose (minBound,maxBound)

instance Random Word8 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary Word8 where
    arbitrary     = choose (minBound,maxBound)

instance Arbitrary B.ByteString where
    arbitrary     = B.pack `fmap` arbitrary

instance Random Word16 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary Word16 where
    arbitrary     = choose (minBound,maxBound)

instance Random Word32 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary Word32 where
    arbitrary     = choose (minBound,maxBound)

instance Arbitrary T.Text where
    arbitrary     = T.pack `fmap` arbitrary

instance Arbitrary TL.Text where
    arbitrary     = TL.pack `fmap` arbitrary

newtype NotEmpty a = NotEmpty { notEmpty :: a }
    deriving (Eq, Ord)

instance Show a => Show (NotEmpty a) where
    show (NotEmpty a) = show a

instance Functor NotEmpty where
    fmap f (NotEmpty a) = NotEmpty (f a)

instance Arbitrary a => Arbitrary (NotEmpty [a]) where
    arbitrary   = sized (\n -> NotEmpty `fmap` (choose (1,n+1) >>= vector))

instance Arbitrary (NotEmpty T.Text) where
    arbitrary   = (fmap T.pack) `fmap` arbitrary

instance Arbitrary (NotEmpty TL.Text) where
    arbitrary   = (fmap TL.pack) `fmap` arbitrary

instance Arbitrary (NotEmpty B.ByteString) where
    arbitrary   = (fmap B.pack) `fmap` arbitrary

data Small = S0  | S1  | S2  | S3  | S4  | S5  | S6  | S7
           | S8  | S9  | S10 | S11 | S12 | S13 | S14 | S15
           | S16 | S17 | S18 | S19 | S20 | S21 | S22 | S23
           | S24 | S25 | S26 | S27 | S28 | S29 | S30 | S31
    deriving (Eq, Ord, Enum, Bounded)

small :: Small -> Int
small = fromEnum

intf f a b = toEnum ((fromEnum a `f` fromEnum b) `mod` 32)

instance Show Small where
    show = show . fromEnum

instance Read Small where
    readsPrec n = map (first toEnum) . readsPrec n

instance Num Small where
    fromInteger = toEnum . fromIntegral
    signum _ = 1
    abs = id
    (+) = intf (+)
    (-) = intf (-)
    (*) = intf (*)

instance Real Small where
    toRational = toRational . fromEnum

instance Integral Small where
    toInteger = toInteger . fromEnum
    quotRem a b = (toEnum x, toEnum y)
        where (x, y) = fromEnum a `quotRem` fromEnum b

instance Random Small where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary Small where
    arbitrary     = choose (minBound,maxBound)

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,h) -> (fromIntegral x, h)
