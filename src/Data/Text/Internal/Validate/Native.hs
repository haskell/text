{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

-- | Native implementation of 'Data.Text.Internal.Validate'.
module Data.Text.Internal.Validate.Native
  ( isValidUtf8ByteStringHaskell
  , isValidUtf8ByteArrayHaskell
  ) where

import Data.Array.Byte (ByteArray(ByteArray))
import Data.ByteString (ByteString)
import GHC.Exts (ByteArray#,Int(I#),indexWord8Array#)
import GHC.Word (Word8(W8#))
import Data.Text.Internal.Encoding.Utf8 (CodePoint(..),DecoderResult(..),utf8DecodeStart,utf8DecodeContinue)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

-- | Native implementation of 'Data.Text.Internal.Validate.isValidUtf8ByteString'.
isValidUtf8ByteStringHaskell :: ByteString -> Bool
isValidUtf8ByteStringHaskell bs = start 0
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

-- | Native implementation of
-- 'Data.Text.Internal.Validate.isValidUtf8ByteArrayUnpinned'
-- and 'Data.Text.Internal.Validate.isValidUtf8ByteArrayPinned'.
isValidUtf8ByteArrayHaskell ::
     ByteArray -- ^ Bytes
  -> Int -- ^ Offset
  -> Int -- ^ Length
  -> Bool
isValidUtf8ByteArrayHaskell (ByteArray b) !off !len = start off
  where
    indexWord8 :: ByteArray# -> Int -> Word8
    indexWord8 !x (I# i) = W8# (indexWord8Array# x i)
    start ix
      | ix >= off + len = True
      | otherwise = case utf8DecodeStart (indexWord8 b ix) of
        Accept{} -> start (ix + 1)
        Reject{} -> False
        Incomplete st _ -> step (ix + 1) st
    step ix st
      | ix >= off + len = False
      -- We do not use decoded code point, so passing a dummy value to save an argument.
      | otherwise = case utf8DecodeContinue (indexWord8 b ix) st (CodePoint 0) of
        Accept{} -> start (ix + 1)
        Reject{} -> False
        Incomplete st' _ -> step (ix + 1) st'
