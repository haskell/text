{-# LANGUAGE MagicHash #-}

-- |
-- Module      : Data.Text.Utf16
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
-- Basic UTF-16 validation and character manipulation.
module Data.Text.Utf16
    (
      chr2
    , validate1
    , validate2
    ) where

import GHC.Exts
import GHC.Word (Word16(..))

chr2 :: Word16 -> Word16 -> Char
chr2 (W16# a#) (W16# b#) = C# (chr# (upper# +# lower# +# 0x10000#))
    where
      x# = word2Int# a#
      y# = word2Int# b#
      upper# = uncheckedIShiftL# (x# -# 0xD800#) 10#
      lower# = y# -# 0xDC00#
{-# INLINE chr2 #-}

validate1    :: Word16 -> Bool
validate1 x1 = (x1 >= 0 && x1 < 0xD800) || (x1 > 0xDFFF && x1 < 0x10000)
{-# INLINE validate1 #-}

validate2       ::  Word16 -> Word16 -> Bool
validate2 x1 x2 = x1 >= 0xD800 && x1 <= 0xDBFF &&
                  x2 >= 0xDC00 && x2 <= 0xDFFF
{-# INLINE validate2 #-}
