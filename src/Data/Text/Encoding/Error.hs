{-# LANGUAGE Safe #-}
-- |
-- Module      : Data.Text.Encoding.Error
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- Types and functions for dealing with encoding and decoding errors
-- in Unicode text.
--
-- The standard functions for encoding and decoding text are strict,
-- which is to say that they throw exceptions on invalid input.  This
-- is often unhelpful on real world input, so alternative functions
-- exist that accept custom handlers for dealing with invalid inputs.
-- These 'OnError' handlers are normal Haskell functions.  You can use
-- one of the presupplied functions in this module, or you can write a
-- custom handler of your own.

module Data.Text.Encoding.Error
    (
    -- * Error handling types
      UnicodeException(..)
    , OnError
    , OnDecodeError
    , OnEncodeError
    -- * Useful error handling functions
    , lenientDecode
    , strictDecode
    , strictEncode
    , ignore
    , replace
    ) where

import Data.Text.Encoding.Common
  ( UnicodeException(..)
  , OnError
  , OnDecodeError
  , OnEncodeError
  , lenientDecode
  , strictDecode
  , strictEncode
  , ignore
  , replace
  )
