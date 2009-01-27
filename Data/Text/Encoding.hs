-- |
-- Module      : Data.Text.Encoding
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009,
--               (c) Duncan Coutts 2009
--
-- License     : BSD-style
-- Maintainer  : rtharper@aftereternity.co.uk, bos@serpentine.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
--
-- Functions for converting 'Text' values to and from 'ByteString',
-- using several common encodings.

module Data.Text.Encoding
    (
    -- * Decoding ByteStrings to Text
      decodeASCII
    , decodeUtf8
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE

    -- * Encoding Text to ByteStrings
    , encodeASCII
    , encodeUtf8
    , encodeUtf16LE
    , encodeUtf16BE
    , encodeUtf32LE
    , encodeUtf32BE
    ) where
    
import Data.ByteString (ByteString)
import qualified Data.Text.Fusion as F
import qualified Data.Text.Encoding.Fusion as E
import Data.Text.Internal (Text)

-- | Decode a 'ByteString' containing 7-bit ASCII encoded text.
decodeASCII :: ByteString -> Text
decodeASCII bs = F.unstream (E.streamASCII bs)
{-# INLINE decodeASCII #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
decodeUtf8 :: ByteString -> Text
decodeUtf8 bs = F.unstream (E.streamUtf8 bs)
{-# INLINE decodeUtf8 #-}

-- | Encode text using a 7-bit ASCII representation. /Note/: non-ASCII
-- characters in the input 'Text' will be /truncated/.
encodeASCII :: Text -> ByteString
encodeASCII txt = E.unstream (E.restreamASCII (F.stream txt))
{-# INLINE encodeASCII #-}

-- | Encode text using UTF-8 encoding.
encodeUtf8 :: Text -> ByteString
encodeUtf8 txt = E.unstream (E.restreamUtf8 (F.stream txt))
{-# INLINE encodeUtf8 #-}

-- | Decode text from little endian UTF-16 encoding.
decodeUtf16LE :: ByteString -> Text
decodeUtf16LE bs = F.unstream (E.streamUtf16LE bs)
{-# INLINE decodeUtf16LE #-}

-- | Decode text from big endian UTF-16 encoding.
decodeUtf16BE :: ByteString -> Text
decodeUtf16BE bs = F.unstream (E.streamUtf16BE bs)
{-# INLINE decodeUtf16BE #-}

-- | Encode text using little endian UTF-16 encoding.
encodeUtf16LE :: Text -> ByteString
encodeUtf16LE txt = E.unstream (E.restreamUtf16LE (F.stream txt))
{-# INLINE encodeUtf16LE #-}

-- | Encode text using big endian UTF-16 encoding.
encodeUtf16BE :: Text -> ByteString
encodeUtf16BE txt = E.unstream (E.restreamUtf16BE (F.stream txt))
{-# INLINE encodeUtf16BE #-}

-- | Decode text from little endian UTF-32 encoding.
decodeUtf32LE :: ByteString -> Text
decodeUtf32LE bs = F.unstream (E.streamUtf32LE bs)
{-# INLINE decodeUtf32LE #-}

-- | Decode text from big endian UTF-32 encoding.
decodeUtf32BE :: ByteString -> Text
decodeUtf32BE bs = F.unstream (E.streamUtf32LE bs)
{-# INLINE decodeUtf32BE #-}

-- | Encode text using little endian UTF-32 encoding.
encodeUtf32LE :: Text -> ByteString
encodeUtf32LE txt = E.unstream (E.restreamUtf32LE (F.stream txt))
{-# INLINE encodeUtf32LE #-}

-- | Encode text using big endian UTF-32 encoding.
encodeUtf32BE :: Text -> ByteString
encodeUtf32BE txt = E.unstream (E.restreamUtf32BE (F.stream txt))
{-# INLINE encodeUtf32BE #-}
