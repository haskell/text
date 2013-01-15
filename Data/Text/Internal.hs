{-# LANGUAGE CPP, DeriveDataTypeable #-}

-- |
-- Module      : Data.Text.Internal
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- A module containing private 'Text' internals. This exposes the
-- 'Text' representation and low level construction functions.
-- Modules which extend the 'Text' system may need to use this module.
--
-- You should not use this module unless you are determined to monkey
-- with the internals, as the functions here do just about nothing to
-- preserve data invariants.  You have been warned!

module Data.Text.Internal
    (
    -- * Types
    -- $internals
      Text(..)
    -- * Construction
    , text
    , textP
    -- * Safety
    , safe
    -- * Code that must be here for accessibility
    , empty
    -- * Utilities
    , firstf
    -- * Debugging
    , showText
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.Bits ((.&.))
import qualified Data.Text.Array as A
import Data.Text.UnsafeChar (ord)
import Data.Typeable (Typeable)

-- | A space efficient, packed, unboxed Unicode text type.
data Text = Text
    {-# UNPACK #-} !A.Array          -- payload (Word16 elements)
    {-# UNPACK #-} !Int              -- offset (units of Word16, not Char)
    {-# UNPACK #-} !Int              -- length (units of Word16, not Char)
    deriving (Typeable)

-- | Smart constructor.
text :: A.Array -> Int -> Int -> Text
text arr off len =
#if defined(ASSERTS)
  let c    = A.unsafeIndex arr off
      alen = A.length arr
  in assert (len >= 0) .
     assert (off >= 0) .
     assert (alen == 0 || len == 0 || off < alen) .
     assert (len == 0 || c < 0xDC00 || c > 0xDFFF) $
#endif
     Text arr off len
{-# INLINE text #-}

-- | /O(1)/ The empty 'Text'.
empty :: Text
empty = Text A.empty 0 0
{-# INLINE [1] empty #-}

-- | Construct a 'Text' without invisibly pinning its byte array in
-- memory if its length has dwindled to zero.
textP :: A.Array -> Int -> Int -> Text
textP arr off len | len == 0  = empty
                  | otherwise = text arr off len
{-# INLINE textP #-}

-- | A useful 'show'-like function for debugging purposes.
showText :: Text -> String
showText (Text arr off len) =
    "Text " ++ show (A.toList arr off len) ++ ' ' :
            show off ++ ' ' : show len

-- | Map a 'Char' to a 'Text'-safe value.
--
-- UTF-16 surrogate code points are not included in the set of Unicode
-- scalar values, but are unfortunately admitted as valid 'Char'
-- values by Haskell.  They cannot be represented in a 'Text'.  This
-- function remaps those code points to the Unicode replacement
-- character (U+FFFD, \'&#xfffd;\'), and leaves other code points
-- unchanged.
safe :: Char -> Char
safe c
    | ord c .&. 0x1ff800 /= 0xd800 = c
    | otherwise                    = '\xfffd'
{-# INLINE safe #-}

-- | Apply a function to the first element of an optional pair.
firstf :: (a -> c) -> Maybe (a,b) -> Maybe (c,b)
firstf f (Just (a, b)) = Just (f a, b)
firstf _  Nothing      = Nothing

-- $internals
--
-- Internally, the 'Text' type is represented as an array of 'Word16'
-- UTF-16 code units. The offset and length fields in the constructor
-- are in these units, /not/ units of 'Char'.
--
-- Invariants that all functions must maintain:
--
-- * Since the 'Text' type uses UTF-16 internally, it cannot represent
--   characters in the reserved surrogate code point range U+D800 to
--   U+DFFF. To maintain this invariant, the 'safe' function maps
--   'Char' values in this range to the replacement character (U+FFFD,
--   \'&#xfffd;\').
--
-- * A leading (or \"high\") surrogate code unit (0xD800–0xDBFF) must
--   always be followed by a trailing (or \"low\") surrogate code unit
--   (0xDC00-0xDFFF). A trailing surrogate code unit must always be
--   preceded by a leading surrogate code unit.
