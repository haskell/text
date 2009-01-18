module QuickCheckUtils where

import Test.QuickCheck
import Test.QuickCheck.Batch

import Data.Char

import Data.Text
import Data.Text.Internal

instance Arbitrary Char where
    arbitrary    = oneof [choose ('\0','\55295'), choose ('\57334','\1114111')]
    coarbitrary c = variant (ord c `rem` 4)

instance Arbitrary Text where
    arbitrary     = pack `fmap` arbitrary
    coarbitrary s = coarbitrary (unpack s)
