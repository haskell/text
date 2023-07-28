{-# LANGUAGE RebindableSyntax, TemplateHaskell #-}

module Tests.RebindableSyntaxTest where

import qualified Data.Text as Text
import Language.Haskell.TH.Syntax (lift)
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty (TestTree, testGroup)
import Prelude (($))

tests :: TestTree
tests = testGroup "RebindableSyntax"
  [ testCase "test" $ assertEqual "a" $(lift (Text.pack "a")) (Text.pack "a")
  ]
