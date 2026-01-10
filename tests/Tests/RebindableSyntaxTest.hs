{-# LANGUAGE CPP, RebindableSyntax, TemplateHaskell #-}

module Tests.RebindableSyntaxTest where

import qualified Data.Text as Text
#if __GLASGOW_HASKELL__ >= 914
import Language.Haskell.TH.Lift (lift)
#else
import Language.Haskell.TH.Syntax (lift)
#endif
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty (TestTree, testGroup)
import Prelude (($))

tests :: TestTree
tests = testGroup "RebindableSyntax"
  [ testCase "test" $ assertEqual "a" $(lift (Text.pack "a")) (Text.pack "a")
  ]
