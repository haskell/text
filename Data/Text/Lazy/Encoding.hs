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
    --, decodeUtf8
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
