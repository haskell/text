-- | Provides a simple main function which runs all the tests
--
module Main
    ( main
    ) where

import Test.Tasty (defaultMain)

import qualified Lift

main :: IO ()
main = defaultMain Lift.tests
