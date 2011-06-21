-- | Provides a simple main function which runs all the tests
--
module Main
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Data.Text.Tests.Properties as Properties
import qualified Data.Text.Tests.Regressions as Regressions

main :: IO ()
main = defaultMain [Properties.tests, Regressions.tests]
