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
    , textP
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
data Text = Text
    {-# UNPACK #-} !(A.Array Word16) -- payload
    {-# UNPACK #-} !Int              -- offset
    {-# UNPACK #-} !Int              -- length
    deriving (Typeable)

-- | Smart constructor.
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

-- | Construct a 'Text' without invisibly pinning its byte array in
-- memory if its length has dwindled to zero.
textP :: A.Array Word16 -> Int -> Int -> Text
textP arr off len | len == 0  = empty
                  | otherwise = text arr off len
{-# INLINE textP #-}

-- | A useful 'show'-like function for debugging purposes.
showText :: Text -> String
showText (Text arr off len) =
    "Text " ++ (show . take (off+len) . A.toList) arr ++ ' ' :
            show off ++ ' ' : show len
