module Data.Text.Utf32 where

import Data.Word (Word32)

validate    :: Word32 -> Bool
validate x1 = (x1 >= 0x0 && x1 < 0xD800) || (x1 > 0xDFFF && x1 <= 0x10FFFF)
