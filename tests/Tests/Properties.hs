-- | QuickCheck properties for the text library.

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tests.Properties
    (
      tests
    ) where

import Test.Tasty (TestTree, testGroup)
import Tests.Properties.Basics (testBasics)
import Tests.Properties.Builder (testBuilder)
import Tests.Properties.Folds (testFolds)
import Tests.Properties.LowLevel (testLowLevel)
import Tests.Properties.Instances (testInstances)
import Tests.Properties.Substrings (testSubstrings)
import Tests.Properties.Read (testRead)
import Tests.Properties.Text (testText)
import Tests.Properties.Transcoding (testTranscoding)

tests :: TestTree
tests =
  testGroup "Properties" [
    testTranscoding,
    testInstances,
    testBasics,
    testFolds,
    testText,
    testSubstrings,
    testBuilder,
    testLowLevel,
    testRead
  ]
