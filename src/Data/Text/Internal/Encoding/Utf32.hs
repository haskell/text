{-# LANGUAGE CPP, MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Encoding.Utf32
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Basic UTF-32 validation.
module Data.Text.Internal.Encoding.Utf32
    (
      validate
    , queryUtf32Bytes
    ) where

import Data.Word (Word32)
import GHC.Exts
import GHC.Word (Word8(..))

#if !MIN_VERSION_base(4,16,0)
-- harmless to import, except for warnings that it is unused.
import Data.Text.Internal.PrimCompat (word8ToWord#)
#endif

validate    :: Word32 -> Bool
validate x1 = x1 < 0xD800 || (x1 > 0xDFFF && x1 <= 0x10FFFF)
{-# INLINE validate #-}

queryUtf32Bytes :: (Eq a, Num a) => a -> Word8 -> Word8 -> Word8 -> Maybe Char
queryUtf32Bytes b0 b1@(W8# w1#) b2@(W8# w2#) (W8# w3#)
  | b0 == 0
  , b1 < 0x11
  , b1 > 0 || b2 < 0xD8 || b2 >= 0xE0 =
    Just $
      C# (chr#
          (orI#
            (orI#
              (word2Int# (shiftL# (word8ToWord# w1#) 16#))
              (word2Int# (shiftL# (word8ToWord# w2#) 8#))
            )
            (word2Int# (word8ToWord# w3#))
          )
        )
  | otherwise = Nothing
{-# INLINE queryUtf32Bytes #-}
