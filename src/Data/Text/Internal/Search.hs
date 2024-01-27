{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Module      : Data.Text.Internal.Search
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast substring search for 'Text', based on work by Boyer, Moore,
-- Horspool, Sunday, and Lundh.
--
-- References:
--
-- * R. S. Boyer, J. S. Moore: A Fast String Searching Algorithm.
--   Communications of the ACM, 20, 10, 762-772 (1977)
--
-- * R. N. Horspool: Practical Fast Searching in Strings.  Software -
--   Practice and Experience 10, 501-506 (1980)
--
-- * D. M. Sunday: A Very Fast Substring Search Algorithm.
--   Communications of the ACM, 33, 8, 132-142 (1990)
--
-- * F. Lundh:
--   <http://web.archive.org/web/20201107074620/http://effbot.org/zone/stringlib.htm The Fast Search Algorithm>. (2006)

module Data.Text.Internal.Search
    (
      indices
    ) where

import qualified Data.Text.Array as A
import Data.Word (Word64, Word8)
import Data.Text.Internal (Text(..))
import Data.Bits ((.|.), (.&.), unsafeShiftL)
import Data.Text.Internal.ArrayUtils (memchr)

data T = {-# UNPACK #-} !Word64 :* {-# UNPACK #-} !Int

-- | /O(n+m)/ Find the offsets of all non-overlapping indices of
-- @needle@ within @haystack@.
--
-- In (unlikely) bad cases, this algorithm's complexity degrades
-- towards /O(n*m)/.
indices :: Text                -- ^ Substring to search for (@needle@)
        -> Text                -- ^ Text to search in (@haystack@)
        -> [Int]
indices needle@(Text narr noff nlen)
  | nlen == 1 = scanOne (A.unsafeIndex narr noff)
  | nlen <= 0 = const []
  | otherwise = indices' needle
{-# INLINE indices #-}

-- | nlen must be >= 2, otherwise nindex causes access violation
indices' :: Text -> Text -> [Int]
indices' (Text narr noff nlen) (Text harr@(A.ByteArray harr#) hoff hlen) = loop (hoff + nlen)
  where
    nlast    = nlen - 1
    !z       = nindex nlast
    nindex k = A.unsafeIndex narr (noff+k)
    buildTable !i !msk !skp
        | i >= nlast           = (msk .|. swizzle z) :* skp
        | otherwise            = buildTable (i+1) (msk .|. swizzle c) skp'
        where !c               = nindex i
              skp' | c == z    = nlen - i - 2
                   | otherwise = skp
    !(mask :* skip) = buildTable 0 0 (nlen-2)

    swizzle :: Word8 -> Word64
    swizzle !k = 1 `unsafeShiftL` (word8ToInt k .&. 0x3f)

    loop !i
      | i > hlen + hoff
      = []
      | A.unsafeIndex harr (i - 1) == z
      = if A.equal narr noff harr (i - nlen) nlen
        then i - nlen - hoff : loop (i + nlen)
        else                   loop (i + skip + 1)
      | i == hlen + hoff
      = []
      | mask .&. swizzle (A.unsafeIndex harr i) == 0
      = loop (i + nlen + 1)
      | otherwise
      = case memchr harr# i (hlen + hoff - i) z of
        -1 -> []
        x  -> loop (i + x + 1)
{-# INLINE indices' #-}

scanOne :: Word8 -> Text -> [Int]
scanOne c (Text harr hoff hlen) = loop 0
  where
    loop !i
      | i >= hlen                        = []
      | A.unsafeIndex harr (hoff+i) == c = i : loop (i+1)
      | otherwise                        = loop (i+1)
{-# INLINE scanOne #-}

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral
