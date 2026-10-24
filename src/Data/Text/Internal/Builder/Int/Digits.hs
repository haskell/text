-- Module:      Data.Text.Internal.Builder.Int.Digits
-- Copyright:   (c) 2013 Bryan O'Sullivan
-- License:     BSD-style
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- This module exists because the C preprocessor does things that we
-- shall not speak of when confronted with Haskell multiline strings.

module Data.Text.Internal.Builder.Int.Digits (digits, Base(..), base10, base16) where

import Data.Bits (finiteBitSize)
import Data.ByteString.Char8 (ByteString, pack)

digits :: ByteString
digits = pack
  "0001020304050607080910111213141516171819\
  \2021222324252627282930313233343536373839\
  \4041424344454647484950515253545556575859\
  \6061626364656667686970717273747576777879\
  \8081828384858687888990919293949596979899"

data Base = Base
  { base :: !Word
  , maxWord :: !Integer
    -- ^ Greatest power of @b@ whose remainders are representable as an @Word@
  , maxDigits :: !Int
    -- ^ Number of base-@b@ digits to represent numbers smaller than @maxWord@
  }

base10 :: Base
base10 = Base
  { base = 10
  , maxWord = 10 ^ maxDigits10
  , maxDigits = maxDigits10
  } where
    -- log_10 2 = 0.301... > 3/10
    -- In general this is an underapproximation, and it is close enough that
    -- the rounding makes it exact for 64-bit words.
    maxDigits10 = finiteBitSize (0 :: Word) * 3 `div` 10

base16 :: Base
base16 = Base
  { base = 16
  , maxWord = 16 ^ maxDigits16
  , maxDigits = maxDigits16
  } where
    maxDigits16 = finiteBitSize (0 :: Word) `div` 4
