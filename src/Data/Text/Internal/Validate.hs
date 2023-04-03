{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- | Test whether or not a sequence of bytes is a valid UTF-8 byte sequence.
-- In the GHC Haskell ecosystem, there are several representations of byte
-- sequences. The only one that the stable @text@ API concerns itself with is
-- 'ByteString'. Part of bytestring-to-text decoding is 'isValidUtf8ByteString',
-- a high-performance UTF-8 validation routine written in C++ with fallbacks
-- for various platforms. The C++ code backing this routine is nontrivial,
-- so in the interest of reuse, this module additionally exports functions
-- for working with the GC-managed @ByteArray@ type. These @ByteArray@
-- functions are not used anywhere else in @text@. They are for the benefit
-- of library and application authors who do not use 'ByteString' but still
-- need to interoperate with @text@.
module Data.Text.Internal.Validate
  (
  -- * ByteString
    isValidUtf8ByteString
  -- * ByteArray
  --
  -- | Is the slice of a byte array a valid UTF-8 byte sequence? These
  -- functions all accept an offset and a length.
  , isValidUtf8ByteArray
  , isValidUtf8ByteArrayUnpinned
  , isValidUtf8ByteArrayPinned
  ) where

import Data.Array.Byte (ByteArray(ByteArray))
import Data.ByteString (ByteString)
import GHC.Exts (isTrue#,isByteArrayPinned#)

#ifdef SIMDUTF
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Text.Internal.ByteStringCompat (withBS)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Data.Text.Internal.Validate.Simd (c_is_valid_utf8_bytearray_safe,c_is_valid_utf8_bytearray_unsafe,c_is_valid_utf8_ptr_unsafe)
#else
import GHC.Exts (ByteArray#)
import Data.Text.Internal.Encoding.Utf8 (CodePoint(..),DecoderResult(..),utf8DecodeStart,utf8DecodeContinue)
import GHC.Exts (Int(I#),indexWord8Array#)
import GHC.Word (Word8(W8#))
import qualified Data.ByteString as B
#if !MIN_VERSION_bytestring(0,11,2)
import qualified Data.ByteString.Unsafe as B
#endif
#endif

-- | Is the ByteString a valid UTF-8 byte sequence?
isValidUtf8ByteString :: ByteString -> Bool
#ifdef SIMDUTF
isValidUtf8ByteString bs = withBS bs $ \fp len -> unsafeDupablePerformIO $
  unsafeWithForeignPtr fp $ \ptr -> (/= 0) <$> c_is_valid_utf8_ptr_unsafe ptr (fromIntegral len)
#else
#if MIN_VERSION_bytestring(0,11,2)
isValidUtf8ByteString = B.isValidUtf8
#else
isValidUtf8ByteString bs = start 0
  where
    start ix
      | ix >= B.length bs = True
      | otherwise = case utf8DecodeStart (B.unsafeIndex bs ix) of
        Accept{} -> start (ix + 1)
        Reject{} -> False
        Incomplete st _ -> step (ix + 1) st
    step ix st
      | ix >= B.length bs = False
      -- We do not use decoded code point, so passing a dummy value to save an argument.
      | otherwise = case utf8DecodeContinue (B.unsafeIndex bs ix) st (CodePoint 0) of
        Accept{} -> start (ix + 1)
        Reject{} -> False
        Incomplete st' _ -> step (ix + 1) st'
#endif
#endif

-- | For pinned byte arrays larger than 128KiB, this switches to the safe FFI
-- so that it does not prevent GC. This threshold (128KiB) was chosen
-- somewhat arbitrarily and may change in the future.
isValidUtf8ByteArray ::
     ByteArray -- ^ Bytes
  -> Int -- ^ Offset
  -> Int -- ^ Length
  -> Bool
isValidUtf8ByteArray b@(ByteArray b#) !off !len
  | len >= 131072 -- 128KiB
  , isTrue# (isByteArrayPinned# b#)
  = isValidUtf8ByteArrayPinned b off len
  | otherwise = isValidUtf8ByteArrayUnpinned b off len

-- | This uses the @unsafe@ FFI. GC waits for all @unsafe@ FFI calls
-- to complete before starting. Consequently, an @unsafe@ FFI call does not
-- run concurrently with GC and is not interrupted by GC. Since relocation
-- cannot happen concurrently with an @unsafe@ FFI call, it is safe
-- to call this function with an unpinned byte array argument.
-- It is also safe to call this with a pinned @ByteArray@ argument.
isValidUtf8ByteArrayUnpinned ::
     ByteArray -- ^ Bytes
  -> Int -- ^ Offset
  -> Int -- ^ Length
  -> Bool
#ifdef SIMDUTF
isValidUtf8ByteArrayUnpinned (ByteArray bs) !off !len =
  unsafeDupablePerformIO $ (/= 0) <$> c_is_valid_utf8_bytearray_unsafe bs (fromIntegral off) (fromIntegral len)
#else
isValidUtf8ByteArrayUnpinned (ByteArray bs) = isValidUtf8ByteArrayHaskell# bs
#endif

-- | This uses the @safe@ FFI. GC may run concurrently with @safe@
-- FFI calls. Consequently, unpinned objects may be relocated while a
-- @safe@ FFI call is executing. The byte array argument /must/ be pinned,
-- and the calling context is responsible for enforcing this. If the
-- byte array is not pinned, this function's behavior is undefined.
isValidUtf8ByteArrayPinned ::
     ByteArray -- ^ Bytes
  -> Int -- ^ Offset
  -> Int -- ^ Length
  -> Bool
#ifdef SIMDUTF
isValidUtf8ByteArrayPinned (ByteArray bs) !off !len =
  unsafeDupablePerformIO $ (/= 0) <$> c_is_valid_utf8_bytearray_safe bs (fromIntegral off) (fromIntegral len)
#else
isValidUtf8ByteArrayPinned (ByteArray bs) = isValidUtf8ByteArrayHaskell# bs
#endif

#ifndef SIMDUTF
isValidUtf8ByteArrayHaskell# ::
     ByteArray# -- ^ Bytes
  -> Int -- ^ Offset
  -> Int -- ^ Length
  -> Bool
isValidUtf8ByteArrayHaskell# b !off !len = start off
  where
    indexWord8 :: ByteArray# -> Int -> Word8
    indexWord8 !x (I# i) = W8# (indexWord8Array# x i)
    start ix
      | ix >= len = True
      | otherwise = case utf8DecodeStart (indexWord8 b ix) of
        Accept{} -> start (ix + 1)
        Reject{} -> False
        Incomplete st _ -> step (ix + 1) st
    step ix st
      | ix >= len = False
      -- We do not use decoded code point, so passing a dummy value to save an argument.
      | otherwise = case utf8DecodeContinue (indexWord8 b ix) st (CodePoint 0) of
        Accept{} -> start (ix + 1)
        Reject{} -> False
        Incomplete st' _ -> step (ix + 1) st'
#endif
