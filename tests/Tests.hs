{-# LANGUAGE CPP #-}

module Main
    ( main
    ) where

import Test.Tasty (defaultMain, testGroup)

import qualified Tests.Lift as Lift
import qualified Tests.Properties as Properties
import qualified Tests.Regressions as Regressions

#if !defined(ASSERTS)
import qualified Tests.Inspection.Strict as InspectionStrict
import qualified Tests.Inspection.Lazy   as InspectionLazy
#endif

main :: IO ()
main = defaultMain $ testGroup "All"
  [ Lift.tests
  , Properties.tests
  , Regressions.tests
#if !defined(ASSERTS)
  , InspectionStrict.tests
  , InspectionLazy.tests
#endif
  ]
