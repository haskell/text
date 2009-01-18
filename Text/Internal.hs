module Text.Internal where

import Data.Array.ST(newArray_,runSTUArray)
import Data.Array.Unboxed(UArray(..))
import Data.Word(Word16(..))

data Text = Text !(UArray Int Word16) {-# UNPACK #-}!Int {-# UNPACK #-}!Int

empty :: Text
empty = Text (runSTUArray (newArray_ (0,0))) 0 0
{-# INLINE [1] empty #-}
