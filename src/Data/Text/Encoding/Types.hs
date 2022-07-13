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
  , chunksDecoderToStream
  )
  where

-- | A decoding result on encoded data.
--
-- @since 2.0.0.1
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
--
-- @since 2.0.0.1
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

-- | Create a stream decoder from a chunks decoder. The resulting
-- stream decoder will return a 'StreamDecodeResult' which contains a
-- continuation function that accepts another section of unencoded
-- data as a continuation of any remaining unencoded data.
--
-- @since 2.0.0.1
chunksDecoderToStream :: Monoid b
  => (b -> b -> DecodeResult t b w) -- ^ Chunk decoder function
  -> b -> StreamDecodeResult t b w
chunksDecoderToStream chunksDecoder = g 0 mempty
  where
    g pos bs0 bs1 =
      let DecodeResult t mW bs1' pos1 = chunksDecoder bs0 bs1
          pos' = pos + pos1
      in
      StreamDecodeResult t mW bs1' pos' $ \ bs2 -> g pos' bs1' bs2
