{-# LANGUAGE FlexibleInstances #-}

module QuickCheckUtils where

import Control.Arrow (first)
import Data.Char (chr)
import Data.Bits ((.&.))
import Data.String (IsString, fromString)
import qualified Data.Text as T
import Data.Text.Foreign (I16)
import qualified Data.Text.Lazy as TL
import System.Random (Random(..), RandomGen)
import Test.QuickCheck hiding ((.&.))
import qualified Data.ByteString as B

instance Random I16 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary I16 where
    arbitrary     = choose (minBound,maxBound)

instance Arbitrary B.ByteString where
    arbitrary     = B.pack `fmap` arbitrary

genUnicode :: IsString a => Gen a
genUnicode = fmap fromString string where
    string = sized $ \n ->
        do k <- choose (0,n)
           sequence [ char | _ <- [1..k] ]
    
    excluding :: [a -> Bool] -> Gen a -> Gen a
    excluding bad gen = loop
      where
        loop = do
          x <- gen
          if or (map ($ x) bad)
            then loop
            else return x
    
    reserved = [lowSurrogate, highSurrogate, noncharacter]
    lowSurrogate c = c >= 0xDC00 && c <= 0xDFFF
    highSurrogate c = c >= 0xD800 && c <= 0xDBFF
    noncharacter c = masked == 0xFFFE || masked == 0xFFFF
      where
        masked = c .&. 0xFFFF 
    
    ascii = choose (0,0x7F)
    plane0 = choose (0xF0, 0xFFFF)
    plane1 = oneof [ choose (0x10000, 0x10FFF)
                   , choose (0x11000, 0x11FFF)
                   , choose (0x12000, 0x12FFF)
                   , choose (0x13000, 0x13FFF)
                   , choose (0x1D000, 0x1DFFF)
                   , choose (0x1F000, 0x1FFFF)
                   ]
    plane2 = oneof [ choose (0x20000, 0x20FFF)
                   , choose (0x21000, 0x21FFF)
                   , choose (0x22000, 0x22FFF)
                   , choose (0x23000, 0x23FFF)
                   , choose (0x24000, 0x24FFF)
                   , choose (0x25000, 0x25FFF)
                   , choose (0x26000, 0x26FFF)
                   , choose (0x27000, 0x27FFF)
                   , choose (0x28000, 0x28FFF)
                   , choose (0x29000, 0x29FFF)
                   , choose (0x2A000, 0x2AFFF)
                   , choose (0x2B000, 0x2BFFF)
                   , choose (0x2F000, 0x2FFFF)
                   ]
    plane14 = choose (0xE0000, 0xE0FFF)
    planes = [ascii, plane0, plane1, plane2, plane14]
    
    char = chr `fmap` excluding reserved (oneof planes)

-- For tests that have O(n^2) running times or input sizes, resize
-- their inputs to the square root of the originals.
unsquare :: (Arbitrary a, Show a, Testable b) => (a -> b) -> Property
unsquare = forAll smallArbitrary

smallArbitrary :: (Arbitrary a, Show a) => Gen a
smallArbitrary = sized $ \n -> resize (smallish n) arbitrary
  where smallish = round . (sqrt :: Double -> Double) . fromIntegral . abs

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary

instance Arbitrary TL.Text where
    arbitrary = (TL.fromChunks . map notEmpty) `fmap` smallArbitrary

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

small :: Integral a => Small -> a
small = fromIntegral . fromEnum

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
