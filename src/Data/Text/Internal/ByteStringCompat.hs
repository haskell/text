{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
module Data.Text.Internal.ByteStringCompat (mkBS, withBS) where

import Data.ByteString.Internal (ByteString (..))
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)

#if !MIN_VERSION_bytestring(0,11,0)
import GHC.ForeignPtr (plusForeignPtr)
#endif

mkBS :: ForeignPtr Word8 -> Int -> ByteString
#if MIN_VERSION_bytestring(0,11,0)
mkBS dfp n = BS dfp n
#else
mkBS dfp n = PS dfp 0 n
#endif
{-# INLINE mkBS #-}

withBS :: ByteString -> (ForeignPtr Word8 -> Int -> r) -> r
#if MIN_VERSION_bytestring(0,11,0)
withBS (BS !sfp !slen)       kont = kont sfp slen
#else
withBS (PS !sfp !soff !slen) kont = kont (plusForeignPtr sfp soff) slen
#endif
{-# INLINE withBS #-}
