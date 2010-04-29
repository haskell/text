{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
-- |
-- Module      : Data.Text.Unsafe
-- Copyright   : (c) Bryan O'Sullivan 2009
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
--
-- A module containing unsafe 'Text' operations, for very very careful
-- use in heavily tested code.
module Data.Text.Unsafe
    (
      inlineInterleaveST
    , inlinePerformIO
    , iter
    , iter_
    , reverseIter
    , unsafeHead
    , unsafeTail
    ) where
     
import Control.Exception (assert)
import Data.Text.Encoding.Utf16 (chr2)
import Data.Text.Internal (Text(..))
import Data.Text.UnsafeChar (unsafeChr)
import GHC.ST (ST(..))
import qualified Data.Text.Array as A
#if defined(__GLASGOW_HASKELL__)
# if __GLASGOW_HASKELL__ >= 611
import GHC.IO (IO(IO))
# else
import GHC.IOBase (IO(IO))
# endif
import GHC.Base (realWorld#)
#endif

-- | /O(1)/ A variant of 'head' for non-empty 'Text'. 'unsafeHead'
-- omits the check for the empty case, so there is an obligation on
-- the programmer to provide a proof that the 'Text' is non-empty.
unsafeHead :: Text -> Char
unsafeHead (Text arr off len)
    | m < 0xD800 || m > 0xDBFF = unsafeChr m
    | otherwise                = chr2 m n
    where m = assert (len > 0) $ A.unsafeIndex arr off
          n = assert (len > 1) $ A.unsafeIndex arr (off+1)
{-# INLINE unsafeHead #-}

-- | /O(1)/ A variant of 'tail' for non-empty 'Text'. 'unsafeHead'
-- omits the check for the empty case, so there is an obligation on
-- the programmer to provide a proof that the 'Text' is non-empty.
unsafeTail :: Text -> Text
unsafeTail t@(Text arr off len) =
    assert (d <= len) $ Text arr (off+d) (len-d)
  where d = iter_ t 0
{-# INLINE unsafeTail #-}

-- | /O(1)/ Iterate one step forwards through a UTF-16 array,
-- returning the current character and the delta to add to give the
-- next offset to iterate at.
iter :: Text -> Int -> (Char,Int)
iter (Text arr off len) i
    | m < 0xD800 || m > 0xDBFF = (unsafeChr m, 1)
    | otherwise                = (chr2 m n,    2)
  where m = assert (i < len)     $ A.unsafeIndex arr j
        n = assert (i + 1 < len) $ A.unsafeIndex arr k
        j = assert (i >= 0)      $ off + i
        k =                        j + 1
{-# INLINE iter #-}

-- | /O(1)/ Iterate one step through a UTF-16 array, returning the
-- delta to add to give the next offset to iterate at.
iter_ :: Text -> Int -> Int
iter_ (Text arr off len) i | m < 0xD800 || m > 0xDBFF = 1
                           | otherwise                = 2
  where m = assert (i >= 0 && i < len) $ A.unsafeIndex arr (off+i)
{-# INLINE iter_ #-}

-- | /O(1)/ Iterate one step backwards through a UTF-16 array,
-- returning the current character and the delta to add (i.e. a
-- negative number) to give the next offset to iterate at.
reverseIter :: Text -> Int -> (Char,Int)
reverseIter (Text arr off len) i
    | m < 0xDC00 || m > 0xDFFF = (unsafeChr m, -1)
    | otherwise                = (chr2 n m,    -2)
  where m = assert (i < len)    $ A.unsafeIndex arr j
        n = assert (i - 1 >= 0) $ A.unsafeIndex arr k
        j = assert (i >= 0)     $ off + i
        k =                       j - 1
{-# INLINE reverseIter #-}

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block. On Hugs this is just @unsafePerformIO@.
--
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif

-- | Allow an 'ST' computation to be deferred lazily. When passed an
-- action of type 'ST' @s@ @a@, the action will only be performed when
-- the value of @a@ is demanded.
--
-- This function is identical to the normal unsafeInterleaveST, but is
-- inlined and hence faster.
--
-- /Note/: This operation is highly unsafe, as it can introduce
-- externally visible non-determinism into an 'ST' action.
inlineInterleaveST :: ST s a -> ST s a
inlineInterleaveST (ST m) = ST $ \ s ->
    let r = case m s of (# _, res #) -> res in (# s, r #)
{-# INLINE inlineInterleaveST #-}
