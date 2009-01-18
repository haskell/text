module Text.Utf32 where

import Data.Bits
import Data.Char
import Data.Word

validate    :: Word32 -> Bool
validate x1 = (x1 >= 0x0 && x1 < 0xD800) || (x1 > 0xDFFF && x1 <= 0x10FFFF)
