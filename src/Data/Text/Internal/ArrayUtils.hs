{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE CPP #-}

module Data.Text.Internal.ArrayUtils (memchr) where

#if defined(PURE_HASKELL)
import qualified Data.Text.Array as A
import Data.List (elemIndex)
#else
import Foreign.C.Types
import System.Posix.Types (CSsize(..))
#endif
import GHC.Exts (ByteArray#)
import Data.Word (Word8)

memchr :: ByteArray# -> Int -> Int -> Word8 -> Int
#if defined(PURE_HASKELL)
memchr arr# off len w =
    let tempBa = A.ByteArray arr#
    in case elemIndex w (A.toList tempBa off len) of
        Nothing -> -1
        Just i -> i
#else
memchr arr# off len w = fromIntegral $ c_memchr arr# (intToCSize off) (intToCSize len) w

intToCSize :: Int -> CSize
intToCSize = fromIntegral


foreign import ccall unsafe "_hs_text_memchr" c_memchr
    :: ByteArray# -> CSize -> CSize -> Word8 -> CSsize
#endif
