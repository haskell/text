{-# LANGUAGE DeriveDataTypeable #-}

module Data.Text.Internal
    (
      Text(..)
    , empty
    ) where

import Data.Array.ST (newArray_,runSTUArray)
import Data.Array.Unboxed (UArray)
import Data.Typeable (Typeable)
import Data.Word (Word16)

data Text = Text {-# UNPACK #-} !(UArray Int Word16) -- payload
                 {-# UNPACK #-} !Int                 -- offset
                 {-# UNPACK #-} !Int                 -- length
            deriving (Typeable)

empty :: Text
empty = Text (runSTUArray (newArray_ (0,0))) 0 0
{-# INLINE [1] empty #-}
