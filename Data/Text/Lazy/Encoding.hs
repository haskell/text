-- |
-- Module      : Data.Text.Lazy.Encoding
-- Copyright   : (c) Bryan O'Sullivan 2009
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
    , decodeUtf8With
    --, decodeUtf16LE
    --, decodeUtf16BE
    --, decodeUtf32LE
    --, decodeUtf32BE

    -- * Encoding Text to ByteStrings
    , encodeUtf8
    , encodeUtf16LE
    , encodeUtf16BE
    , encodeUtf32LE
    , encodeUtf32BE
    ) where

import qualified Data.ByteString.Lazy as B
import Data.Text.Encoding.Error (OnDecodeError, strictDecode)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Fusion as F
import Data.Text.Lazy.Internal (Text(..), chunk, foldrChunks)
import qualified Data.Text.Lazy.Encoding.Fusion as E

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

-- | Encode text using UTF-8 encoding.
encodeUtf8 :: Text -> B.ByteString
encodeUtf8 txt = E.unstream (E.restreamUtf8 (F.stream txt))
{-# INLINE encodeUtf8 #-}

-- | Encode text using little endian UTF-16 encoding.
encodeUtf16LE :: Text -> B.ByteString
encodeUtf16LE txt = B.fromChunks (foldrChunks ((:) . TE.encodeUtf16LE) [] txt)
{-# INLINE encodeUtf16LE #-}

-- | Encode text using big endian UTF-16 encoding.
encodeUtf16BE :: Text -> B.ByteString
encodeUtf16BE txt = B.fromChunks (foldrChunks ((:) . TE.encodeUtf16BE) [] txt)
{-# INLINE encodeUtf16BE #-}

-- | Encode text using little endian UTF-32 encoding.
encodeUtf32LE :: Text -> B.ByteString
encodeUtf32LE txt = B.fromChunks (foldrChunks ((:) . TE.encodeUtf32LE) [] txt)
{-# INLINE encodeUtf32LE #-}

-- | Encode text using big endian UTF-32 encoding.
encodeUtf32BE :: Text -> B.ByteString
encodeUtf32BE txt = B.fromChunks (foldrChunks ((:) . TE.encodeUtf32BE) [] txt)
{-# INLINE encodeUtf32BE #-}
