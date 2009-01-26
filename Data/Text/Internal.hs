{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Data.Text
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009,
--               (c) Duncan Coutts 2009
--
-- License     : BSD-style
-- Maintainer  : rtharper@aftereternity.co.uk, bos@serpentine.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- Semi-public internals.  Most users should not need to use this
-- module.

module Data.Text.Internal
    (
    -- * Types
      Text(..)
    -- * Code that must be here for accessibility
    , empty
    ) where

import qualified Data.Text.Array as A
import Data.Typeable (Typeable)
import Data.Word (Word16)

-- | A space efficient, packed, unboxed Unicode text type.
data Text = Text {-# UNPACK #-} !(A.Array Word16) -- payload
                 {-# UNPACK #-} !Int              -- offset
                 {-# UNPACK #-} !Int              -- length
            deriving (Typeable)

-- | /O(1)/ The empty 'Text'.
empty :: Text
empty = Text A.empty 0 0
{-# INLINE [1] empty #-}
