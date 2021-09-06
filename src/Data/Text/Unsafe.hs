{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
-- |
-- Module      : Data.Text.Unsafe
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : portable
--
-- A module containing unsafe 'Text' operations, for very very careful
-- use in heavily tested code.
module Data.Text.Unsafe
    (
      inlineInterleaveST
    , inlinePerformIO
    , unsafeDupablePerformIO
    , Iter(..)
    , iter
    , iterArray
    , iter_
    , reverseIter
    , reverseIterArray
    , reverseIter_
    , unsafeHead
    , unsafeTail
    , lengthWord8
    , takeWord8
    , dropWord8
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
import GHC.Stack (HasCallStack)
#endif
import Data.Text.Internal.Encoding.Utf8 (chr2, chr3, chr4, utf8LengthByLeader)
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Unsafe (inlineInterleaveST, inlinePerformIO)
import Data.Text.Internal.Unsafe.Char (unsafeChr8)
import qualified Data.Text.Array as A
import GHC.IO (unsafeDupablePerformIO)

-- | /O(1)/ A variant of 'head' for non-empty 'Text'. 'unsafeHead'
-- omits the check for the empty case, so there is an obligation on
-- the programmer to provide a proof that the 'Text' is non-empty.
unsafeHead :: Text -> Char
unsafeHead (Text arr off _len) = case utf8LengthByLeader m0 of
    1 -> unsafeChr8 m0
    2 -> chr2 m0 m1
    3 -> chr3 m0 m1 m2
    _ -> chr4 m0 m1 m2 m3
    where m0 = A.unsafeIndex arr off
          m1 = A.unsafeIndex arr (off+1)
          m2 = A.unsafeIndex arr (off+2)
          m3 = A.unsafeIndex arr (off+3)
{-# INLINE unsafeHead #-}

-- | /O(1)/ A variant of 'tail' for non-empty 'Text'. 'unsafeTail'
-- omits the check for the empty case, so there is an obligation on
-- the programmer to provide a proof that the 'Text' is non-empty.
unsafeTail :: Text -> Text
unsafeTail t@(Text arr off len) =
#if defined(ASSERTS)
    assert (d <= len) $
#endif
    Text arr (off+d) (len-d)
  where d = iter_ t 0
{-# INLINE unsafeTail #-}

data Iter = Iter {-# UNPACK #-} !Char {-# UNPACK #-} !Int
  deriving (Show)

-- | /O(1)/ Iterate (unsafely) one step forwards through a UTF-8
-- array, returning the current character and the delta to add to give
-- the next offset to iterate at.
iter ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> Int -> Iter
iter (Text arr off _len) i = iterArray arr (off + i)
{-# INLINE iter #-}

-- | @since 2.0
iterArray :: A.Array -> Int -> Iter
iterArray arr j = Iter chr l
  where m0 = A.unsafeIndex arr j
        m1 = A.unsafeIndex arr (j+1)
        m2 = A.unsafeIndex arr (j+2)
        m3 = A.unsafeIndex arr (j+3)
        l = utf8LengthByLeader m0
        chr = case l of
            1 -> unsafeChr8 m0
            2 -> chr2 m0 m1
            3 -> chr3 m0 m1 m2
            _ -> chr4 m0 m1 m2 m3
{-# INLINE iterArray #-}

-- | /O(1)/ Iterate one step through a UTF-8 array, returning the
-- delta to add to give the next offset to iterate at.
iter_ :: Text -> Int -> Int
iter_ (Text arr off _len) i = utf8LengthByLeader m
  where m = A.unsafeIndex arr (off+i)
{-# INLINE iter_ #-}

-- | /O(1)/ Iterate one step backwards through a UTF-8 array,
-- returning the current character and the delta to add (i.e. a
-- negative number) to give the next offset to iterate at.
reverseIter :: Text -> Int -> Iter
reverseIter (Text arr off _len) i = reverseIterArray arr (off + i)
{-# INLINE reverseIter #-}

-- | @since 2.0
reverseIterArray :: A.Array -> Int -> Iter
reverseIterArray arr j
    | m0 <  0x80 = Iter (unsafeChr8 m0) (-1)
    | m1 >= 0xC0 = Iter (chr2 m1 m0) (-2)
    | m2 >= 0xC0 = Iter (chr3 m2 m1 m0) (-3)
    | otherwise  = Iter (chr4 m3 m2 m1 m0) (-4)
  where m0 = A.unsafeIndex arr j
        m1 = A.unsafeIndex arr (j-1)
        m2 = A.unsafeIndex arr (j-2)
        m3 = A.unsafeIndex arr (j-3)
{-# INLINE reverseIterArray #-}

-- | /O(1)/ Iterate one step backwards through a UTF-8 array,
-- returning the delta to add (i.e. a negative number) to give the
-- next offset to iterate at.
--
-- @since 1.1.1.0
reverseIter_ :: Text -> Int -> Int
reverseIter_ (Text arr off _len) i
    | m0 <  0x80 = -1
    | m1 >= 0xC0 = -2
    | m2 >= 0xC0 = -3
    | otherwise  = -4
  where m0 = A.unsafeIndex arr j
        m1 = A.unsafeIndex arr (j-1)
        m2 = A.unsafeIndex arr (j-2)
        j = off + i
{-# INLINE reverseIter_ #-}

-- | /O(1)/ Return the length of a 'Text' in units of 'Word8'.  This
-- is useful for sizing a target array appropriately before using
-- 'unsafeCopyToPtr'.
--
-- @since 2.0
lengthWord8 :: Text -> Int
lengthWord8 (Text _arr _off len) = len
{-# INLINE lengthWord8 #-}

-- | /O(1)/ Unchecked take of 'k' 'Word8's from the front of a 'Text'.
--
-- @since 2.0
takeWord8 :: Int -> Text -> Text
takeWord8 k (Text arr off _len) = Text arr off k
{-# INLINE takeWord8 #-}

-- | /O(1)/ Unchecked drop of 'k' 'Word8's from the front of a 'Text'.
--
-- @since 2.0
dropWord8 :: Int -> Text -> Text
dropWord8 k (Text arr off len) = Text arr (off+k) (len-k)
{-# INLINE dropWord8 #-}
