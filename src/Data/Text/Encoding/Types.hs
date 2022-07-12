{-# LANGUAGE Safe #-}
-- |
-- Module      : Data.Text.Encoding.Types
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- Types to indicate the result of an attempt to decode data.

module Data.Text.Encoding.Types
  ( DecodeResult(..)
  , StreamDecodeResult(..)
  )
  where

-- | A decoding result on encoded data.
data DecodeResult t b w = DecodeResult
  !t          -- ^ The decoded data up to an incomplete code point at
              -- the end of the input data, an invalid word, or to the
              -- end of the input.
  !(Maybe w)  -- ^ If an invalid code point was encountered.
  !b          -- ^ The remaining undecoded data. If an invald code
              -- point was encountered, this is after that code point.
  !Int        -- ^ Byte position of remaining undecoded data.
  deriving (Eq, Ord, Show, Read)

-- | A decoding result on encoded data.
data StreamDecodeResult t b w = StreamDecodeResult
  !t          -- ^ The decoded data up to an incomplete code point at
              -- the end of the input data, an invalid word, or to the
              -- end of the input.
  !(Maybe w)  -- ^ If an invalid code point was encountered.
  !b          -- ^ The remaining undecoded data. If an invald code
              -- point was encountered, this is after that code point.
  !Int        -- ^ Byte position of remaining undecoded data. This is
              -- treated as if all the data fed to previous invocations
              -- of the continations where one continuous feed.
  (b -> StreamDecodeResult t b w) -- ^ Continuation to accept the next
                                  -- span of data to be decoded with
                                  -- the remaining unencoded data.
