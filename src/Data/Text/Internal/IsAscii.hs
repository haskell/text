{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
#if defined(PURE_HASKELL)
{-# LANGUAGE BangPatterns #-}
#endif

{-# OPTIONS_HADDOCK not-home #-}

-- | Implements 'isAscii', using efficient C routines by default.
--
-- Similarly implements asciiPrefixLength, used internally in Data.Text.Encoding.
module Data.Text.Internal.IsAscii where

#if defined(PURE_HASKELL)
import Prelude hiding (all)
import qualified Data.Char as Char
import qualified Data.ByteString as BS
import Data.Text.Unsafe (iter, Iter(..))
#else
import Data.Text.Internal.ByteStringCompat (withBS)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.Ptr (Ptr, plusPtr)
import GHC.Base (ByteArray#)
import Prelude (Bool(..), Int, (==), ($), IO, (<$>))
import qualified Data.Text.Array as A
#endif
import Data.ByteString (ByteString)
import Data.Text.Internal (Text(..))
import qualified Prelude as P

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
isAscii = all Char.isAscii

-- | (Re)implemented to avoid circular dependency on Data.Text.
all :: (Char -> Bool) -> Text -> Bool
all p t@(Text _ _ len) = go 0
  where
    go i | i >= len = True
         | otherwise =
             let !(Iter c j) = iter t i
             in p c && go (i+j)
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

-- | Length of the longest ASCII prefix.
asciiPrefixLength :: ByteString -> Int
#if defined(PURE_HASKELL)
asciiPrefixLength = BS.length P.. BS.takeWhile (P.< 0x80)
#else
asciiPrefixLength bs = unsafeDupablePerformIO $ withBS bs $ \ fp len ->
  unsafeWithForeignPtr fp $ \src -> do
    P.fromIntegral <$> c_is_ascii src (src `plusPtr` len)
#endif
{-# INLINE asciiPrefixLength #-}

#if !defined(PURE_HASKELL)
foreign import ccall unsafe "_hs_text_is_ascii_offset" c_is_ascii_offset
    :: ByteArray# -> CSize -> CSize -> CSize

foreign import ccall unsafe "_hs_text_is_ascii" c_is_ascii
    :: Ptr Word8 -> Ptr Word8 -> IO CSize
#endif
