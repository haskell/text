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
    -- * Construction
    , text
    -- * Code that must be here for accessibility
    , empty
    -- * Debugging
    , showText
    ) where

import Control.Exception (assert)
import qualified Data.Text.Array as A
import Data.Typeable (Typeable)
import Data.Word (Word16)

-- | A space efficient, packed, unboxed Unicode text type.
data Text = Text {
      textArray :: {-# UNPACK #-} !(A.Array Word16) -- payload
    , textOffset :: {-# UNPACK #-} !Int              -- offset
    , textLength :: {-# UNPACK #-} !Int              -- length
    } deriving (Typeable)

text :: A.Array Word16 -> Int -> Int -> Text
text arr off len =
    assert (len >= 0) .
    assert (off >= 0) .
    assert (alen == 0 || off < alen) .
    assert (len == 0 || c < 0xDC00 || c > 0xDFFF) $
    Text arr off len
  where c    = A.unsafeIndex arr off
        alen = A.length arr
{-# INLINE text #-}

-- | /O(1)/ The empty 'Text'.
empty :: Text
empty = Text A.empty 0 0
{-# INLINE [1] empty #-}

showText :: Text -> String
showText (Text arr off len) =
    "Text " ++ (show . take (off+len) . A.toList) arr ++ ' ' :
            show off ++ ' ' : show len
