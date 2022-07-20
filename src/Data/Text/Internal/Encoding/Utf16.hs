{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.Text.Internal.Encoding.Utf16
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Basic UTF-16 validation and character manipulation.
module Data.Text.Internal.Encoding.Utf16
    (
      chr2
    , validate1
    , validate2
    , Utf16Result(..)
    , queryUtf16Bytes
    ) where

import Data.Bits ((.&.))
import GHC.Exts
import GHC.Word (Word16(..), Word8(..))

#if !MIN_VERSION_base(4,16,0)
-- harmless to import, except for warnings that it is unused.
import Data.Text.Internal.PrimCompat ( word16ToWord#, word8ToWord# )
#endif

chr2 :: Word16 -> Word16 -> Char
chr2 (W16# a#) (W16# b#) = C# (chr# (upper# +# lower# +# 0x10000#))
    where
      !x# = word2Int# (word16ToWord# a#)
      !y# = word2Int# (word16ToWord# b#)
      !upper# = uncheckedIShiftL# (x# -# 0xD800#) 10#
      !lower# = y# -# 0xDC00#
{-# INLINE chr2 #-}

validate1    :: Word16 -> Bool
validate1 x1 = x1 < 0xD800 || x1 > 0xDFFF
{-# INLINE validate1 #-}

validate2       ::  Word16 -> Word16 -> Bool
validate2 x1 x2 = x1 >= 0xD800 && x1 <= 0xDBFF &&
                  x2 >= 0xDC00 && x2 <= 0xDFFF
{-# INLINE validate2 #-}

data Utf16Result
  = OneWord16 Char
  | TwoWord16 (Word8 -> Word8 -> Maybe Char)
  | Invalid16

queryUtf16Bytes :: Word8 -> Word8 -> Utf16Result
queryUtf16Bytes b0@(W8# w0#) (W8# w1#)
  | b0 < 0xD8 || b0 >= 0xE0 = OneWord16 $ C# (chr# (orI# (word2Int# (shiftL# (word8ToWord# w0#) 8#)) (word2Int# (word8ToWord# w1#))))
  -- 110110xx: start of surrogate pair
  | b0 .&. 0xFC == 0xD8 = TwoWord16 $ \ b2@(W8# w2#) (W8# w3#) ->
    if b2 .&. 0xFC == 0xDC
      -- valid surrogate
      then Just $
        C# (chr# (
          (orI#
            (orI#
              (orI#
                (word2Int# (shiftL# (int2Word# (andI# 0x3# (word2Int# (word8ToWord# w0#)))) 18#))
                (word2Int# (shiftL# (word8ToWord# w1#) 10#))
              )
              (word2Int# (shiftL# (int2Word# (andI# 0x3# (word2Int# (word8ToWord# w2#)))) 8#)))
            (word2Int# (word8ToWord# w3#))) +# 0x10000#
        ))
      else Nothing
  | otherwise = Invalid16
{-# INLINE queryUtf16Bytes #-}
