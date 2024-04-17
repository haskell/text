{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : Data.Text.Internal.Builder
-- License     : BSD-style (see LICENSE)
-- Stability   : experimental
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- @since 2.0.2

module Data.Text.Internal.StrictBuilder
  ( StrictTextBuilder(..)
  , StrictBuilder
  , toText
  , fromChar
  , fromText

    -- * Unsafe
    -- $unsafe
  , unsafeFromByteString
  , unsafeFromWord8
  ) where

import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.Functor (void)
import Data.Word (Word8)
import Data.ByteString (ByteString)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.Text.Internal (Text(..), empty, safe)
import Data.Text.Internal.ByteStringCompat (withBS)
import Data.Text.Internal.Encoding.Utf8 (utf8Length)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import qualified Data.ByteString as B
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Unsafe.Char as Char

-- | A delayed representation of strict 'Text'.
--
-- @since 2.1.2
data StrictTextBuilder = StrictTextBuilder
  { sbLength :: {-# UNPACK #-} !Int
  , sbWrite :: forall s. A.MArray s -> Int -> ST s ()
  }

-- | A delayed representation of strict 'Text'.
--
-- @since 2.0.2
{-# DEPRECATED StrictBuilder "Use StrictTextBuilder instead" #-}
type StrictBuilder = StrictTextBuilder

-- | Use 'StrictBuilder' to build 'Text'.
--
-- @since 2.0.2
toText :: StrictTextBuilder -> Text
toText (StrictTextBuilder 0 _) = empty
toText (StrictTextBuilder n write) = runST (do
  dst <- A.new n
  write dst 0
  arr <- A.unsafeFreeze dst
  pure (Text arr 0 n))

-- | Concatenation of 'StrictBuilder' is right-biased:
-- the right builder will be run first. This allows a builder to
-- run tail-recursively when it was accumulated left-to-right.
instance Semigroup StrictTextBuilder where
  (<>) = appendRStrictBuilder

instance Monoid StrictTextBuilder where
  mempty = emptyStrictBuilder
  mappend = (<>)

emptyStrictBuilder :: StrictTextBuilder
emptyStrictBuilder = StrictTextBuilder 0 (\_ _ -> pure ())

appendRStrictBuilder :: StrictTextBuilder -> StrictTextBuilder -> StrictTextBuilder
appendRStrictBuilder (StrictTextBuilder 0 _) b2 = b2
appendRStrictBuilder b1 (StrictTextBuilder 0 _) = b1
appendRStrictBuilder (StrictTextBuilder n1 write1) (StrictTextBuilder n2 write2) =
  StrictTextBuilder (n1 + n2) (\dst ofs -> do
    write2 dst (ofs + n1)
    write1 dst ofs)

copyFromByteString :: A.MArray s -> Int -> ByteString -> ST s ()
copyFromByteString dst ofs src = withBS src $ \ srcFPtr len ->
  unsafeIOToST $ unsafeWithForeignPtr srcFPtr $ \ srcPtr -> do
    unsafeSTToIO $ A.copyFromPointer dst ofs srcPtr len

-- | Copy a 'ByteString'.
--
-- Unsafe: This may not be valid UTF-8 text.
--
-- @since 2.0.2
unsafeFromByteString :: ByteString -> StrictTextBuilder
unsafeFromByteString bs =
  StrictTextBuilder (B.length bs) (\dst ofs -> copyFromByteString dst ofs bs)

-- |
-- @since 2.0.2
{-# INLINE fromChar #-}
fromChar :: Char -> StrictTextBuilder
fromChar c =
  StrictTextBuilder (utf8Length c) (\dst ofs -> void (Char.unsafeWrite dst ofs (safe c)))

-- $unsafe
-- For internal purposes, we abuse 'StrictBuilder' as a delayed 'Array' rather
-- than 'Text': it may not actually be valid 'Text'.

-- | Unsafe: This may not be valid UTF-8 text.
--
-- @since 2.0.2
unsafeFromWord8 :: Word8 -> StrictTextBuilder
unsafeFromWord8 !w =
  StrictTextBuilder 1 (\dst ofs -> A.unsafeWrite dst ofs w)

-- | Copy 'Text' in a 'StrictBuilder'
--
-- @since 2.0.2
fromText :: Text -> StrictTextBuilder
fromText (Text src srcOfs n) = StrictTextBuilder n (\dst dstOfs ->
  A.copyI n dst dstOfs src srcOfs)
