-- |
-- Module      : Data.Text.Lazy.Encoding
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
    --, decodeUtf16LE
    --, decodeUtf16BE
    --, decodeUtf32LE
    --, decodeUtf32BE

    -- * Encoding Text to ByteStrings
    --, encodeUtf8
    --, encodeUtf16LE
    --, encodeUtf16BE
    --, encodeUtf32LE
    --, encodeUtf32BE
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Fusion as F
import qualified Data.Text.Lazy.Encoding.Fusion as E

-- | Decode a 'ByteString' containing UTF-8 encoded text.
decodeUtf8 :: ByteString -> Text
decodeUtf8 bs = F.unstream (E.streamUtf8 bs)
{-# INLINE decodeUtf8 #-}
