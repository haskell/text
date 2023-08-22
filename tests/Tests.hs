{-# LANGUAGE CPP #-}

module Main
    ( main
    ) where

import Test.Tasty (defaultMain, testGroup)

import qualified Tests.Lift as Lift
import qualified Tests.Properties as Properties
import qualified Tests.Regressions as Regressions
import qualified Tests.ShareEmpty as ShareEmpty
import qualified Tests.RebindableSyntaxTest as RST

main :: IO ()
main = defaultMain $ testGroup "All"
  [ Lift.tests
  , Properties.tests
  , Regressions.tests
  , ShareEmpty.tests
  , RST.tests
  ]
