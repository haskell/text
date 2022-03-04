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
  , testCase "strict0" $ assertEqual "strict0"
      $(lift ("f\0o\1o\2" :: S.Text))
      ("f\0o\1o\2" :: S.Text)
  , testCase "strict-nihao" $ assertEqual "strict-nihao"
      $(lift ("\20320\22909" :: S.Text))
      ("\20320\22909" :: S.Text)
  , testCase "lazy" $ assertEqual "lazy"
      $(lift ("foo" :: L.Text))
      ("foo" :: L.Text)
  , testCase "lazy0" $ assertEqual "lazy0"
      $(lift ("f\0o\1o\2" :: L.Text))
      ("f\0o\1o\2" :: L.Text)
  , testCase "lazy-nihao" $ assertEqual "lazy-nihao"
      $(lift ("\20320\22909" :: L.Text))
      ("\20320\22909" :: L.Text)
  ]
