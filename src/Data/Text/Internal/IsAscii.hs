{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Implements 'isAscii', using efficient C routines by default.
module Data.Text.Internal.IsAscii where

import Prelude (Bool(..), Int, (==))
import qualified Prelude as P
import Data.Text.Internal (Text(..))
import Foreign.C.Types
import qualified Data.Text.Array as A
import GHC.Base (ByteArray#)

-- | \O(n)\ Test whether 'Text' contains only ASCII code-points (i.e. only
--   U+0000 through U+007F).
--
-- This is a more efficient version of @'all' 'Data.Char.isAscii'@.
--
-- >>> isAscii ""
-- True
--
-- >>> isAscii "abc\NUL"
-- True
--
-- >>> isAscii "abcd€"
-- False
--
-- prop> isAscii t == all (< '\x80') t
--
-- @since 2.0.2
isAscii :: Text -> Bool
#if defined(PURE_HASKELL)
isAscii = P.const False
#else
cSizeToInt :: CSize -> Int
cSizeToInt = P.fromIntegral
{-# INLINE cSizeToInt #-}

intToCSize :: Int -> CSize
intToCSize = P.fromIntegral

isAscii (Text (A.ByteArray arr) off len) =
    cSizeToInt (c_is_ascii_offset arr (intToCSize off) (intToCSize len)) == len
#endif
{-# INLINE isAscii #-}


#if !defined(PURE_HASKELL)
foreign import ccall unsafe "_hs_text_is_ascii_offset" c_is_ascii_offset
    :: ByteArray# -> CSize -> CSize -> CSize
#endif
