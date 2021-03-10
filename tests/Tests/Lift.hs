{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Tests.Lift
  ( tests
  )
  where

import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import Language.Haskell.TH.Syntax (lift)
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "TH lifting Text"
  [ testCase "strict" $ assertEqual "strict"
      $(lift ("foo" :: S.Text))
      ("foo" :: S.Text)
  , testCase "lazy" $ assertEqual "lazy"
      $(lift ("foo" :: L.Text))
      ("foo" :: L.Text)
  ]
