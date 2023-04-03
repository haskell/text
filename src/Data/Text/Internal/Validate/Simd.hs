{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | Validate that a byte sequence is UTF-8-encoded text. All of these
-- functions return zero when the byte sequence is not UTF-8-encoded text,
-- and they return an unspecified non-zero value when the byte sequence
-- is UTF-8-encoded text.
--
-- Variants are provided for both @ByteArray#@ and @Ptr@. Additionally,
-- variants are provided that use both the @safe@ and @unsafe@ FFI.
--
-- If compiling with SIMDUTF turned off, this module exports nothing.
module Data.Text.Internal.Validate.Simd
  ( c_is_valid_utf8_ptr_unsafe
  , c_is_valid_utf8_ptr_safe
  , c_is_valid_utf8_bytearray_unsafe
  , c_is_valid_utf8_bytearray_safe
  ) where

import Data.Word (Word8)
import Foreign.C.Types (CSize(..),CInt(..))
import GHC.Exts (Ptr,ByteArray#)

foreign import ccall unsafe "_hs_text_is_valid_utf8" c_is_valid_utf8_ptr_unsafe
    :: Ptr Word8 -- ^ Bytes
    -> CSize -- ^ Length
    -> IO CInt
foreign import ccall safe "_hs_text_is_valid_utf8" c_is_valid_utf8_ptr_safe
    :: Ptr Word8 -- ^ Bytes
    -> CSize -- ^ Length
    -> IO CInt
foreign import ccall unsafe "_hs_text_is_valid_utf8_offset" c_is_valid_utf8_bytearray_unsafe
    :: ByteArray# -- ^ Bytes
    -> CSize -- ^ Offset into bytes
    -> CSize -- ^ Length
    -> IO CInt
foreign import ccall safe "_hs_text_is_valid_utf8_offset" c_is_valid_utf8_bytearray_safe
    :: ByteArray# -- ^ Bytes
    -> CSize -- ^ Offset into bytes
    -> CSize -- ^ Length
    -> IO CInt
