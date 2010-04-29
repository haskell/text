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
    --  decodeASCII
      decodeUtf8
    , decodeUtf8With
    --, decodeUtf16LE
    --, decodeUtf16BE
    --, decodeUtf32LE
    --, decodeUtf32BE

    -- * Encoding Text to ByteStrings
    , encodeUtf8
    --, encodeUtf16LE
    --, encodeUtf16BE
    --, encodeUtf32LE
    --, encodeUtf32BE
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Encoding.Error (OnDecodeError, strictDecode)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Fusion as F
import qualified Data.Text.Lazy.Encoding.Fusion as E

-- | Decode a 'ByteString' containing UTF-8 encoded text.
decodeUtf8With :: OnDecodeError -> ByteString -> Text
decodeUtf8With onErr bs = F.unstream (E.streamUtf8 onErr bs)
{-# INLINE decodeUtf8With #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With strictDecode
{-# INLINE decodeUtf8 #-}

-- | Encode text using UTF-8 encoding.
encodeUtf8 :: Text -> ByteString
encodeUtf8 txt = E.unstream (E.restreamUtf8 (F.stream txt))
{-# INLINE encodeUtf8 #-}
