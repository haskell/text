{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# LANGUAGE Safe #-}
-- |
-- Module      : Data.Text.Encoding.Common
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- Common functions and types for both lazy and strict encoding and
-- decoding including error handling.
--
-- The standard functions for encoding and decoding text are strict,
-- which is to say that they throw exceptions on invalid input.  This
-- is often unhelpful on real world input, so alternative functions
-- exist that accept custom handlers for dealing with invalid inputs.
-- These 'OnError' handlers are normal Haskell functions.  You can use
-- one of the presupplied functions in this module, or you can write a
-- custom handler of your own.

module Data.Text.Encoding.Common
  (
  -- * Full-service result types and functions
    DecodeResult(..)
  , DecodeResultHandler
  , chunksDecoderToStream
  -- * Error handling types
  , UnicodeException(..)
  , OnError
  , OnDecodeError
  , OnEncodeError
  -- * Useful error handling functions
  , lenientDecode
  , strictDecode
  , strictEncode
  , ignore
  , replace
  )
  where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Numeric (showHex)

-- | A decoding result on encoded data.
--
-- @since 2.0.1
data DecodeResult t b w = DecodeResult
  !t          -- ^ The decoded data up to an incomplete code point at
              -- the end of the input data, an invalid word, or to the
              -- end of the input.
  (Maybe w)  -- ^ If an invalid code point was encountered.
  !b          -- ^ The remaining undecoded data. If an invald code
              -- point was encountered, this is after that code point.
  !Int        -- ^ Byte position of remaining undecoded data.
  deriving (Eq, Ord, Show, Read)

-- | The funtion type for handling successful decodings.
--
-- @since 2.0.1
type DecodeResultHandler t w b b' r
  = t           -- ^ The decoded data up to an incomplete code point at
                -- the end of the input data, an invalid word, or to
                -- the end of the input.
  -> Maybe w    -- ^ If an invalid code point was encountered.
  -> b          -- ^ The remaining undecoded data. If an invald code
                -- point was encountered, this is after that code
                -- point.
  -> Int        -- ^ Byte position of remaining undecoded data. This
                -- is treated as if all the data fed to previous
                -- invocations of the continations were one continuous
                -- feed.
  -> (b' -> r)  -- ^ Continuation to accept the next span of data to be
                -- decoded with the remaining unencoded data.
  -> r          -- ^ Result of the continuation.

-- | Create a stream decoder from a chunks decoder. The resulting
-- stream decoder accepts a 'DecodeResultHandler' to process the decode
-- result. The continuations accept another section of unencoded data
-- as a continuation of any remaining unencoded data.
--
-- @since 2.0.1
chunksDecoderToStream :: Monoid b
  => (b -> b' -> DecodeResult t b w)  -- ^ Chunks decoder
  -> b'                               -- ^ Encoded data
  -> DecodeResultHandler t w b b' r   -- ^ Result continuation
  -> r                                -- ^ Continuation result
chunksDecoderToStream chunksDecoder bs f =
  g mempty bs 0
  where
    g bs0 bs1 pos0 =
      let DecodeResult t mW bs1' pos1 = chunksDecoder bs0 bs1
          pos = pos0 + pos1
      in
      f t mW bs1' pos $ \ bs2 -> g bs1' bs2 pos

-- | Function type for handling a coding error.  It is supplied with
-- two inputs:
--
-- * A 'String' that describes the error.
--
-- * The input value that caused the error.  If the error arose
--   because the end of input was reached or could not be identified
--   precisely, this value will be 'Nothing'.
--
-- If the handler returns a value wrapped with 'Just', that value will
-- be used in the output as the replacement for the invalid input.  If
-- it returns 'Nothing', no value will be used in the output.
--
-- Should the handler need to abort processing, it should use 'error'
-- or 'throw' an exception (preferably a 'UnicodeException').  It may
-- use the description provided to construct a more helpful error
-- report.
type OnError a b = String -> Maybe a -> Maybe b

-- | A handler for a decoding error.
type OnDecodeError = OnError Word8 Char

-- | A handler for an encoding error.
{-# DEPRECATED OnEncodeError "This exception is never used in practice, and will be removed." #-}
type OnEncodeError = OnError Char Word8

-- | An exception type for representing Unicode encoding errors.
data UnicodeException =
    DecodeError String (Maybe Word8)
    -- ^ Could not decode a byte sequence because it was invalid under
    -- the given encoding, or ran out of input in mid-decode.
  | EncodeError String (Maybe Char)
    -- ^ Tried to encode a character that could not be represented
    -- under the given encoding, or ran out of input in mid-encode.
    deriving (Eq, Typeable)

{-# DEPRECATED EncodeError "This constructor is never used, and will be removed." #-}

showUnicodeException :: UnicodeException -> String
showUnicodeException (DecodeError desc (Just w))
    = "Cannot decode byte '\\x" ++ showHex w ("': " ++ desc)
showUnicodeException (DecodeError desc Nothing)
    = "Cannot decode input: " ++ desc
showUnicodeException (EncodeError desc (Just c))
    = "Cannot encode character '\\x" ++ showHex (fromEnum c) ("': " ++ desc)
showUnicodeException (EncodeError desc Nothing)
    = "Cannot encode input: " ++ desc

instance Show UnicodeException where
    show = showUnicodeException

instance Exception UnicodeException

instance NFData UnicodeException where
    rnf (DecodeError desc w) = rnf desc `seq` rnf w `seq` ()
    rnf (EncodeError desc c) = rnf desc `seq` rnf c `seq` ()

-- | Throw a 'UnicodeException' if decoding fails.
strictDecode :: OnDecodeError
strictDecode desc c = throw (DecodeError desc c)

-- | Replace an invalid input byte with the Unicode replacement
-- character U+FFFD.
lenientDecode :: OnDecodeError
lenientDecode _ _ = Just '\xfffd'

-- | Throw a 'UnicodeException' if encoding fails.
{-# DEPRECATED strictEncode "This function always throws an exception, and will be removed." #-}
strictEncode :: OnEncodeError
strictEncode desc c = throw (EncodeError desc c)

-- | Ignore an invalid input, substituting nothing in the output.
ignore :: OnError a b
ignore _ _ = Nothing

-- | Replace an invalid input with a valid output.
replace :: b -> OnError a b
replace c _ _ = Just c
