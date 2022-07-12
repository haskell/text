-- Simple test script for #450
-- try running a TH splice which depends on text
-- Run directly by CI
{-# LANGUAGE TemplateHaskell #-}

import Debug.Trace
import Data.Text

main :: IO ()
main = return ()

$( traceM (unpack $ pack "test") >> pure [] )
