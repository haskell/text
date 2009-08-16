{-# LANGUAGE FlexibleInstances #-}

module QuickCheckUtils where

import Data.Int (Int64)
import Data.Word (Word8, Word16, Word32)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Random (Random(..), RandomGen)
import Test.QuickCheck (Arbitrary(..), choose, oneof, sized, variant, vector)
import qualified Data.ByteString as B

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,h) -> (fromIntegral x, h)

instance Random Int64 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary Int64 where
    arbitrary     = choose (minBound,maxBound)
    coarbitrary c = variant (fromEnum c `rem` 4)

instance Random Word8 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary Word8 where
    arbitrary     = choose (minBound,maxBound)
    coarbitrary c = variant (fromEnum c `rem` 4)

instance Arbitrary B.ByteString where
    arbitrary     = B.pack `fmap` arbitrary
    coarbitrary s = coarbitrary (B.unpack s)

instance Random Word16 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary Word16 where
    arbitrary     = choose (minBound,maxBound)
    coarbitrary c = variant (fromEnum c `rem` 4)

instance Random Word32 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary Word32 where
    arbitrary     = choose (minBound,maxBound)
    coarbitrary c = variant (fromEnum c `rem` 4)

instance Arbitrary Char where
    arbitrary     = oneof [choose ('\0','\55295'), choose ('\57344','\1114111')]
    coarbitrary c = variant (fromEnum c `rem` 4)

instance Arbitrary T.Text where
    arbitrary     = T.pack `fmap` arbitrary
    coarbitrary s = coarbitrary (T.unpack s)

instance Arbitrary TL.Text where
    arbitrary     = TL.pack `fmap` arbitrary
    coarbitrary s = coarbitrary (TL.unpack s)

newtype NotEmpty a = NotEmpty { notEmpty :: a }
    deriving (Eq, Ord, Show)

instance Functor NotEmpty where
    fmap f (NotEmpty a) = NotEmpty (f a)

instance Arbitrary a => Arbitrary (NotEmpty [a]) where
    arbitrary   = sized (\n -> NotEmpty `fmap` (choose (1,n+1) >>= vector))
    coarbitrary = coarbitrary . notEmpty

instance Arbitrary (NotEmpty T.Text) where
    arbitrary   = (fmap T.pack) `fmap` arbitrary
    coarbitrary = coarbitrary . notEmpty

instance Arbitrary (NotEmpty TL.Text) where
    arbitrary   = (fmap TL.pack) `fmap` arbitrary
    coarbitrary = coarbitrary . notEmpty

instance Arbitrary (NotEmpty B.ByteString) where
    arbitrary   = (fmap B.pack) `fmap` arbitrary
    coarbitrary = coarbitrary . notEmpty
