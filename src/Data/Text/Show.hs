{-# LANGUAGE CPP, MagicHash #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Data.Text.Show
-- Copyright   : (c) 2009-2015 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC

module Data.Text.Show
    (
      addrLen
    , singleton
    , unpack
    , unpackCString#
    , unpackCStringAscii#
    ) where

import Control.Monad.ST (ST, runST)
import Data.Text.Internal (Text(..), empty, safe, pack)
import Data.Text.Internal.Encoding.Utf8 (utf8Length)
import Data.Text.Internal.Fusion (stream)
import Data.Text.Internal.Unsafe.Char (unsafeWrite)
import GHC.Exts (Ptr(..), Int(..), Addr#, indexWord8OffAddr#)
import GHC.Word (Word8(..))
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Fusion.Common as S
#if !MIN_VERSION_ghc_prim(0,7,0)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize(..))
#endif

import qualified GHC.CString as GHC

#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

instance Show Text where
    showsPrec p ps r = showsPrec p (unpack ps) r

-- | /O(n)/ Convert a 'Text' into a 'String'.
unpack ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> String
unpack = S.unstreamList . stream
{-# INLINE [1] unpack #-}

-- | /O(n)/ Convert a null-terminated
-- <https://en.wikipedia.org/wiki/UTF-8#Modified_UTF-8 modified UTF-8>
-- (but with a standard UTF-8 representation of characters from supplementary planes)
-- string to a 'Text'. Counterpart to 'GHC.unpackCStringUtf8#'.
-- No validation is performed, malformed input can lead to memory access violation.
--
-- @since 1.2.1.1
unpackCString# :: Addr# -> Text
unpackCString# addr# = runST $ do
  let l = addrLen addr#
      at (I# i#) = W8# (indexWord8OffAddr# addr# i#)
  marr <- A.new l
  let go srcOff@(at -> w8) dstOff
        | srcOff >= l
        = return dstOff
        -- Surrogate halves take 3 bytes and are replaced by \xfffd (also 3 bytes long).
        -- Cf. Data.Text.Internal.safe
        | w8 == 0xed, at (srcOff + 1) >= 0xa0 = do
          A.unsafeWrite marr  dstOff      0xef
          A.unsafeWrite marr (dstOff + 1) 0xbf
          A.unsafeWrite marr (dstOff + 2) 0xbd
          go (srcOff + 3) (dstOff + 3)
        -- Byte sequence "\xc0\x80" is used to represent NUL
        | w8 == 0xc0, at (srcOff + 1) == 0x80
        = A.unsafeWrite marr dstOff 0  >> go (srcOff + 2) (dstOff + 1)
        | otherwise
        = A.unsafeWrite marr dstOff w8 >> go (srcOff + 1) (dstOff + 1)
  actualLen <- go 0 0
  A.shrinkM marr actualLen
  arr <- A.unsafeFreeze marr
  return $ Text arr 0 actualLen

-- When a module contains many literal strings, 'unpackCString#' can easily
-- bloat generated code to insane size. There is also very little to gain
-- from inlining. Thus explicit NOINLINE is desired.
{-# NOINLINE unpackCString# #-}

-- | /O(n)/ Convert a null-terminated ASCII string to a 'Text'.
-- Counterpart to 'GHC.unpackCString#'.
-- No validation is performed, malformed input can lead to memory access violation.
--
-- @since 2.0
unpackCStringAscii# :: Addr# -> Text
unpackCStringAscii# addr# = Text ba 0 l
  where
    l = addrLen addr#
    ba = runST $ do
      marr <- A.new l
      A.copyFromPointer marr 0 (Ptr addr#) l
      A.unsafeFreeze marr
{-# NOINLINE unpackCStringAscii# #-}

addrLen :: Addr# -> Int
#if MIN_VERSION_ghc_prim(0,7,0)
addrLen addr# = I# (GHC.cstringLength# addr#)
#else
addrLen addr# = fromIntegral (c_strlen (Ptr addr#))

foreign import capi unsafe "string.h strlen" c_strlen :: CString -> CSize
#endif

{-# RULES "TEXT literal" forall a.
    pack (GHC.unpackCString# a) = unpackCStringAscii# a #-}

{-# RULES "TEXT literal UTF8" forall a.
    pack (GHC.unpackCStringUtf8# a) = unpackCString# a #-}

{-# RULES "TEXT empty literal"
    pack [] = empty #-}

{-# RULES "TEXT singleton literal" forall a.
    pack [a] = singleton a #-}

-- | /O(1)/ Convert a character into a Text.
-- Performs replacement on invalid scalar values.
singleton ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Char -> Text
singleton c = Text (A.run x) 0 len
  where x :: ST s (A.MArray s)
        x = do arr <- A.new len
               _ <- unsafeWrite arr 0 d
               return arr
        len = utf8Length d
        d = safe c
{-# NOINLINE singleton #-}
