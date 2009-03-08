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
     streamUtf8
    --, streamUtf16LE
    --, streamUtf16BE
    --, streamUtf32LE
    --, streamUtf32BE

    -- * Unstreaming
    --, unstream

    , module Data.Text.Encoding.Fusion.Common
    ) where

import Data.ByteString.Lazy.Internal (ByteString(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Text.Encoding.Fusion.Common
import Data.Text.Fusion (Step(..), Stream(..))
import Data.Text.Fusion.Internal (PairS(..))
import Data.Text.UnsafeChar (unsafeChr, unsafeChr8, unsafeChr32)
import qualified Data.Text.Encoding.Utf8 as U8

unknownLength :: Int
unknownLength = 4

-- | /O(n)/ Convert a 'ByteString' into a 'Stream Char', using UTF-8
-- encoding.
streamUtf8 :: ByteString -> Stream Char
streamUtf8 bs0 = Stream next (bs0 :!: 0) unknownLength
    where
      {-# INLINE next #-}
      next (c@(Chunk bs rest) :!: i)
          | i >= l = next (rest :!: 0)
          | U8.validate1 x1 = Yield (unsafeChr8 x1) (c :!: i+1)
          | i+1 < l && U8.validate2 x1 x2 = Yield (U8.chr2 x1 x2) (c :!: i+2)
          | i+2 < l && U8.validate3 x1 x2 x3 = Yield (U8.chr3 x1 x2 x3) (c :!: i+3)
          | i+3 < l && U8.validate4 x1 x2 x3 x4 = Yield (U8.chr4 x1 x2 x3 x4) (c :!: i+4)
          | otherwise = encodingError "UTF-8"
          where
            x1 = idx i
            x2 = idx (i + 1)
            x3 = idx (i + 2)
            x4 = idx (i + 3)
            idx = B.unsafeIndex bs
            l = B.length bs
      next (Empty :!: _) = Done
{-# INLINE [0] streamUtf8 #-}

encodingError :: String -> a
encodingError encoding =
    error $ "Data.Text.Lazy.Encoding.Fusion: Bad " ++ encoding ++ " stream"
