{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Check that the definitions that are partial crash in the expected ways or
-- return sensible defaults.
module Tests.Properties.CornerCases (testCornerCases) where

import Control.Exception
import Data.Either
import Data.Semigroup
import Data.Text
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Tests.QuickCheckUtils ()

testCornerCases :: TestTree
testCornerCases =
  testGroup
    "corner cases"
    [ testGroup
        "stimes"
        $ let specimen = stimes :: Integer -> Text -> Text
          in  [ testProperty
                  "given a negative number, return empty text"
                  $ \(Negative number) text -> specimen number text == ""
              , testProperty
                  "given a number that does not fit into Int, evaluate to error call"
                  $ \(NonNegative number) text ->
                    (ioProperty . fmap isLeft . try @ErrorCall . evaluate) $
                      specimen
                        (fromIntegral (number :: Int) + fromIntegral (maxBound :: Int) + 1)
                        text
              ]
    ]
