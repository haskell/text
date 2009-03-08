-- |
-- Module      : Data.Text.Lazy.Encoding.Fusion
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : rtharper@aftereternity.co.uk, bos@serpentine.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
--
-- Fusible 'Stream'-oriented functions for converting between lazy
-- 'Text' and several common encodings.

module Data.Text.Lazy.Encoding.Fusion
    (
    -- * Streaming
    --  streamASCII
    --, streamUtf8
    --, streamUtf16LE
    --, streamUtf16BE
    --, streamUtf32LE
    --, streamUtf32BE

    -- * Unstreaming
    --, unstream

      module Data.Text.Encoding.Fusion.Common
    ) where

import Data.Text.Encoding.Fusion.Common
