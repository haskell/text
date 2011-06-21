-- | Provides a simple main function which runs all the tests
--
module Main
    ( main
    ) where

import Test.Framework (defaultMain)

import Data.Text.Tests.Properties (tests)

main :: IO ()
main = defaultMain tests
