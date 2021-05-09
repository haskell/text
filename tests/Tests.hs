-- | Provides a simple main function which runs all the tests
--
module Main
    ( main
    ) where

import Test.Tasty (defaultMain, testGroup)

import qualified Tests.Lift as Lift
import qualified Tests.Properties as Properties
import qualified Tests.Regressions as Regressions

import qualified Tests.Inspection.Strict as InspectionStrict
import qualified Tests.Inspection.Lazy   as InspectionLazy

main :: IO ()
main = defaultMain $ testGroup "All"
  [ Lift.tests
  , Properties.tests
  , Regressions.tests
  , InspectionStrict.tests
  , InspectionLazy.tests
  ]
