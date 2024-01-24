{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
#if defined(PURE_HASKELL)
{-# LANGUAGE BangPatterns #-}
#endif

{-# OPTIONS_HADDOCK not-home #-}

-- | Implements 'reverse', using efficient C routines by default.
module Data.Text.Internal.Reverse (reverse, reverseNonEmpty) where

#if !defined(PURE_HASKELL)
import GHC.Exts as Exts
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Foreign.C.Types (CSize(..))
#else
import Control.Monad.ST (ST)
import Data.Text.Internal.Encoding.Utf8 (utf8LengthByLeader)
#endif
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif
import Prelude hiding (reverse)
import Data.Text.Internal (Text(..), empty)
import Control.Monad.ST (runST)
import qualified Data.Text.Array as A

-- | /O(n)/ Reverse the characters of a string.
--
-- Example:
--
-- $setup
-- >>> T.reverse "desrever"
-- "reversed"
reverse ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> Text
reverse (Text _ _ 0) = empty
reverse t            = reverseNonEmpty t
{-# INLINE reverse #-}

-- | /O(n)/ Reverse the characters of a string.
-- Assume that the @Text@ is non-empty. The returned @Text@ is guaranteed to be non-empty.
reverseNonEmpty ::
  Text -> Text
#if defined(PURE_HASKELL)
reverseNonEmpty (Text src off len) = runST $ do
    dest <- A.new len
    _ <- reversePoints src off dest len
    result <- A.unsafeFreeze dest
    pure $ Text result 0 len

-- Step 0:
--
-- Input:  R E D R U M
--         ^
--         x
-- Output: _ _ _ _ _ _
--                     ^
--                     y
--
-- Step 1:
--
-- Input:  R E D R U M
--           ^
--           x
--
-- Output: _ _ _ _ _ R
--                   ^
--                   y
reversePoints
    :: A.Array -- ^ Input array
    -> Int -- ^ Input index
    -> A.MArray s -- ^ Output array
    -> Int -- ^ Output index
    -> ST s ()
reversePoints src xx dest yy = go xx yy where
    go !_ y | y <= 0 = pure ()
    go x y =
        let pLen = utf8LengthByLeader (A.unsafeIndex src x)
            -- The next y is also the start of the current point in the output
            yNext = y - pLen
        in do
            A.copyI pLen dest yNext src x
            go (x + pLen) yNext
#else
reverseNonEmpty (Text (A.ByteArray ba) off len) = runST $ do
    marr@(A.MutableByteArray mba) <- A.new len
    unsafeIOToST $ c_reverse mba ba (fromIntegral off) (fromIntegral len)
    brr <- A.unsafeFreeze marr
    return $ Text brr 0 len
#endif
{-# INLINE reverseNonEmpty #-}

#if !defined(PURE_HASKELL)
-- | The input buffer (src :: ByteArray#, off :: CSize, len :: CSize)
-- must specify a valid UTF-8 sequence, this condition is not checked.
foreign import ccall unsafe "_hs_text_reverse" c_reverse
    :: Exts.MutableByteArray# s -> ByteArray# -> CSize -> CSize -> IO ()
#endif

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.Internal.Reverse as T
