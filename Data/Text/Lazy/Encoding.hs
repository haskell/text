-- |
-- Module      : Data.Text.Lazy.Encoding
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
--
-- Functions for converting lazy 'Text' values to and from lazy
-- 'ByteString', using several standard encodings.
--
-- To make use of a much larger variety of encodings, use the @text-icu@
-- package.

module Data.Text.Lazy.Encoding
    (
    -- * Decoding ByteStrings to Text
      decodeASCII
    , decodeUtf8
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE
    -- ** Controllable error handling
    , decodeUtf8With
    , decodeUtf16LEWith
    , decodeUtf16BEWith
    , decodeUtf32LEWith
    , decodeUtf32BEWith

    -- * Encoding Text to ByteStrings
    , encodeUtf8
    , encodeUtf16LE
    , encodeUtf16BE
    , encodeUtf32LE
    , encodeUtf32BE
    ) where

import Data.Text.Encoding.Error (OnDecodeError, strictDecode)
import Data.Text.Lazy.Internal (Text(..), chunk, foldrChunks)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding.Fusion as E
import qualified Data.Text.Lazy.Fusion as F

-- | Decode a 'ByteString' containing 7-bit ASCII encoded text.
decodeASCII :: B.ByteString -> Text
decodeASCII bs = foldr (chunk . TE.decodeASCII) Empty (B.toChunks bs)
{-# INLINE decodeASCII #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
decodeUtf8With :: OnDecodeError -> B.ByteString -> Text
decodeUtf8With onErr bs = F.unstream (E.streamUtf8 onErr bs)
{-# INLINE decodeUtf8With #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
decodeUtf8 :: B.ByteString -> Text
decodeUtf8 = decodeUtf8With strictDecode
{-# INLINE decodeUtf8 #-}

encodeUtf8 :: Text -> B.ByteString
encodeUtf8 (Chunk c cs) = B.Chunk (TE.encodeUtf8 c) (encodeUtf8 cs)
encodeUtf8 Empty        = B.Empty

-- | Decode text from little endian UTF-16 encoding.
decodeUtf16LEWith :: OnDecodeError -> B.ByteString -> Text
decodeUtf16LEWith onErr bs = F.unstream (E.streamUtf16LE onErr bs)
{-# INLINE decodeUtf16LEWith #-}

-- | Decode text from little endian UTF-16 encoding.
decodeUtf16LE :: B.ByteString -> Text
decodeUtf16LE = decodeUtf16LEWith strictDecode
{-# INLINE decodeUtf16LE #-}

-- | Decode text from big endian UTF-16 encoding.
decodeUtf16BEWith :: OnDecodeError -> B.ByteString -> Text
decodeUtf16BEWith onErr bs = F.unstream (E.streamUtf16BE onErr bs)
{-# INLINE decodeUtf16BEWith #-}

-- | Decode text from big endian UTF-16 encoding.
decodeUtf16BE :: B.ByteString -> Text
decodeUtf16BE = decodeUtf16BEWith strictDecode
{-# INLINE decodeUtf16BE #-}

-- | Encode text using little endian UTF-16 encoding.
encodeUtf16LE :: Text -> B.ByteString
encodeUtf16LE txt = B.fromChunks (foldrChunks ((:) . TE.encodeUtf16LE) [] txt)
{-# INLINE encodeUtf16LE #-}

-- | Encode text using big endian UTF-16 encoding.
encodeUtf16BE :: Text -> B.ByteString
encodeUtf16BE txt = B.fromChunks (foldrChunks ((:) . TE.encodeUtf16BE) [] txt)
{-# INLINE encodeUtf16BE #-}

-- | Decode text from little endian UTF-32 encoding.
decodeUtf32LEWith :: OnDecodeError -> B.ByteString -> Text
decodeUtf32LEWith onErr bs = F.unstream (E.streamUtf32LE onErr bs)
{-# INLINE decodeUtf32LEWith #-}

-- | Decode text from little endian UTF-32 encoding.
decodeUtf32LE :: B.ByteString -> Text
decodeUtf32LE = decodeUtf32LEWith strictDecode
{-# INLINE decodeUtf32LE #-}

-- | Decode text from big endian UTF-32 encoding.
decodeUtf32BEWith :: OnDecodeError -> B.ByteString -> Text
decodeUtf32BEWith onErr bs = F.unstream (E.streamUtf32BE onErr bs)
{-# INLINE decodeUtf32BEWith #-}

-- | Decode text from big endian UTF-32 encoding.
decodeUtf32BE :: B.ByteString -> Text
decodeUtf32BE = decodeUtf32BEWith strictDecode
{-# INLINE decodeUtf32BE #-}

-- | Encode text using little endian UTF-32 encoding.
encodeUtf32LE :: Text -> B.ByteString
encodeUtf32LE txt = B.fromChunks (foldrChunks ((:) . TE.encodeUtf32LE) [] txt)
{-# INLINE encodeUtf32LE #-}

-- | Encode text using big endian UTF-32 encoding.
encodeUtf32BE :: Text -> B.ByteString
encodeUtf32BE txt = B.fromChunks (foldrChunks ((:) . TE.encodeUtf32BE) [] txt)
{-# INLINE encodeUtf32BE #-}
