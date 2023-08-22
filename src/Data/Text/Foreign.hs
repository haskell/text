{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, MagicHash #-}
-- |
-- Module      : Data.Text.Foreign
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- Support for using 'Text' data with native code via the Haskell
-- foreign function interface.

module Data.Text.Foreign
    (
    -- * Interoperability with native code
    -- $interop
      I8
    -- * Safe conversion functions
    , fromPtr
    , fromPtr0
    , useAsPtr
    , asForeignPtr
    -- ** Encoding as UTF-8
    , withCString
    , peekCStringLen
    , withCStringLen
    -- * Unsafe conversion code
    , lengthWord8
    , unsafeCopyToPtr
    -- * Low-level manipulation
    -- $lowlevel
    , dropWord8
    , takeWord8
    ) where

import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCStringLen)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Internal (Text(..), empty)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Data.Text.Show (addrLen)
import Data.Text.Unsafe (lengthWord8)
import Data.Word (Word8)
import Foreign.C.String (CString, CStringLen)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrArray)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Exts (Ptr(..))
import qualified Data.Text.Array as A

-- $interop
--
-- The 'Text' type is implemented using arrays that are not guaranteed
-- to have a fixed address in the Haskell heap. All communication with
-- native code must thus occur by copying data back and forth.
--
-- The 'Text' type's internal representation is UTF-8.
-- To interoperate with native libraries that use different
-- internal representations, such as UTF-16 or UTF-32, consider using
-- the functions in the 'Data.Text.Encoding' module.

-- | A type representing a number of UTF-8 code units.
--
-- @since 2.0
newtype I8 = I8 Int
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

-- | /O(n)/ Create a new 'Text' from a 'Ptr' 'Word8' by copying the
-- contents of the array.
fromPtr :: Ptr Word8           -- ^ source array
        -> I8                  -- ^ length of source array (in 'Word8' units)
        -> IO Text
fromPtr _   (I8 0)   = pure empty
fromPtr ptr (I8 len) = unsafeSTToIO $ do
  dst <- A.new len
  A.copyFromPointer dst 0 ptr len
  arr <- A.unsafeFreeze dst
  return $! Text arr 0 len

-- | /O(n)/ Create a new 'Text' from a 'Ptr' 'Word8' by copying the
-- contents of the NUL-terminated array.
--
-- @since 2.0.1
fromPtr0 :: Ptr Word8           -- ^ source array
         -> IO Text
fromPtr0 ptr@(Ptr addr#) = fromPtr ptr (fromIntegral (addrLen addr#))

-- $lowlevel
--
-- Foreign functions that use UTF-8 internally may return indices in
-- units of 'Word8' instead of characters.  These functions may
-- safely be used with such indices, as they will adjust offsets if
-- necessary to preserve the validity of a Unicode string.

-- | /O(1)/ Return the prefix of the 'Text' of @n@ 'Word8' units in
-- length.
--
-- If @n@ would cause the 'Text' to end inside a code point, the
-- end of the prefix will be advanced by several additional 'Word8' units
-- to maintain its validity.
--
-- @since 2.0
takeWord8 :: I8 -> Text -> Text
takeWord8 = (fst .) . splitAtWord8

-- | /O(1)/ Return the suffix of the 'Text', with @n@ 'Word8' units
-- dropped from its beginning.
--
-- If @n@ would cause the 'Text' to begin inside a code point, the
-- beginning of the suffix will be advanced by several additional 'Word8'
-- unit to maintain its validity.
--
-- @since 2.0
dropWord8 :: I8 -> Text -> Text
dropWord8 = (snd .) . splitAtWord8

splitAtWord8 :: I8 -> Text -> (Text, Text)
splitAtWord8 (I8 n) t@(Text arr off len)
    | n <= 0               = (empty, t)
    | n >= len || m >= len = (t, empty)
    | otherwise            = (Text arr off m, Text arr (off+m) (len-m))
  where
    m | w0 <  0x80 = n   -- last char is ASCII
      | w0 >= 0xF0 = n+3 -- last char starts 4-byte sequence
      | w0 >= 0xE0 = n+2 -- last char starts 3-byte sequence
      | w0 >= 0xC0 = n+1 -- last char starts 2-byte sequence
      | w1 >= 0xF0 = n+2 -- pre-last char starts 4-byte sequence
      | w1 >= 0xE0 = n+1 -- pre-last char starts 3-byte sequence
      | w1 >= 0xC0 = n   -- pre-last char starts 2-byte sequence
      | w2 >= 0xF0 = n+1 -- pre-pre-last char starts 4-byte sequence
      | otherwise  = n   -- pre-pre-last char starts 3-byte sequence
    w0 = A.unsafeIndex arr (off+n-1)
    w1 = A.unsafeIndex arr (off+n-2)
    w2 = A.unsafeIndex arr (off+n-3)

-- | /O(n)/ Copy a 'Text' to an array.  The array is assumed to be big
-- enough to hold the contents of the entire 'Text'.
unsafeCopyToPtr :: Text -> Ptr Word8 -> IO ()
unsafeCopyToPtr (Text arr off len) ptr = unsafeSTToIO $ A.copyToPointer arr off ptr len

-- | /O(n)/ Perform an action on a temporary, mutable copy of a
-- 'Text'.  The copy is freed as soon as the action returns.
useAsPtr :: Text -> (Ptr Word8 -> I8 -> IO a) -> IO a
useAsPtr t@(Text _arr _off len) action =
    allocaBytes len $ \buf -> do
      unsafeCopyToPtr t buf
      action (castPtr buf) (I8 len)

-- | /O(n)/ Make a mutable copy of a 'Text'.
asForeignPtr :: Text -> IO (ForeignPtr Word8, I8)
asForeignPtr t@(Text _arr _off len) = do
  fp <- mallocForeignPtrArray len
  unsafeWithForeignPtr fp $ unsafeCopyToPtr t
  return (fp, I8 len)

-- | Marshal a 'Text' into a C string with a trailing NUL byte,
-- encoded as UTF-8 in temporary storage.
--
-- The temporary storage is freed when the subcomputation terminates
-- (either normally or via an exception), so the pointer to the
-- temporary storage must /not/ be used after this function returns.
--
-- @since 2.0.1
withCString :: Text -> (CString -> IO a) -> IO a
withCString t@(Text _arr _off len) action =
  allocaBytes (len + 1) $ \buf -> do
    unsafeCopyToPtr t buf
    pokeByteOff buf len (0 :: Word8)
    action (castPtr buf)

-- | /O(n)/ Decode a C string with explicit length, which is assumed
-- to have been encoded as UTF-8. If decoding fails, a
-- 'UnicodeException' is thrown.
--
-- @since 1.0.0.0
peekCStringLen :: CStringLen -> IO Text
peekCStringLen cs = do
  bs <- unsafePackCStringLen cs
  return $! decodeUtf8 bs

-- | Marshal a 'Text' into a C string encoded as UTF-8 in temporary
-- storage, with explicit length information. The encoded string may
-- contain NUL bytes, and is not followed by a trailing NUL byte.
--
-- The temporary storage is freed when the subcomputation terminates
-- (either normally or via an exception), so the pointer to the
-- temporary storage must /not/ be used after this function returns.
--
-- @since 1.0.0.0
withCStringLen :: Text -> (CStringLen -> IO a) -> IO a
withCStringLen t act = unsafeUseAsCStringLen (encodeUtf8 t) act
