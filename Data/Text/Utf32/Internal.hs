module Data.Text.Utf32.Internal where

import Data.Array.Unboxed
import Data.Word

data Text = Text !(UArray Int Word32) !Int !Int
