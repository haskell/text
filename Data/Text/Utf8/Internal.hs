module Text.Utf8.Internal where
    
import Data.Array.Unboxed
import Data.Word

data Text = Text !(UArray Int Word8) !Int !Int