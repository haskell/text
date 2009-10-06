{-# LANGUAGE BangPatterns, Rank2Types #-}

-- |
-- Module      : Data.Text.Lazy.Encoding.Fusion
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtharper@aftereternity.co.uk, 
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
    , unstream

    , module Data.Text.Encoding.Fusion.Common
    ) where

import Data.ByteString.Lazy.Internal (ByteString(..), defaultChunkSize)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Text.Encoding.Fusion.Common
import Data.Text.Encoding.Error
import Data.Text.Fusion (Step(..), Stream(..))
import Data.Text.Fusion.Internal (M(..), PairS(..), S(..))
import Data.Text.Fusion.Size
import Data.Text.UnsafeChar (unsafeChr8)
import Data.Word (Word8)
import qualified Data.Text.Encoding.Utf8 as U8
import System.IO.Unsafe (unsafePerformIO)
import Foreign.ForeignPtr (withForeignPtr, ForeignPtr)
import Foreign.Storable (pokeByteOff)
import Data.ByteString.Internal (mallocByteString, memcpy)
import Control.Exception (assert)
import qualified Data.ByteString.Internal as B

-- | /O(n)/ Convert a lazy 'ByteString' into a 'Stream Char', using
-- UTF-8 encoding.
streamUtf8 :: OnDecodeError -> ByteString -> Stream Char
streamUtf8 onErr bs0 = Stream next (bs0 :*: empty :*: 0) unknownSize
    where
      empty = S N N N N
      {-# INLINE next #-}
      next (bs@(Chunk ps _) :*: S N _ _ _ :*: i)
          | i < len && U8.validate1 a =
              Yield (unsafeChr8 a) (bs :*: empty :*: i+1)
          | i + 1 < len && U8.validate2 a b =
              Yield (U8.chr2 a b) (bs :*: empty :*: i+2)
          | i + 2 < len && U8.validate3 a b c =
              Yield (U8.chr3 a b c) (bs :*: empty :*: i+3)
          | i + 4 < len && U8.validate4 a b c d =
              Yield (U8.chr4 a b c d) (bs :*: empty :*: i+4)
          where len = B.length ps
                a = B.unsafeIndex ps i
                b = B.unsafeIndex ps (i+1)
                c = B.unsafeIndex ps (i+2)
                d = B.unsafeIndex ps (i+3)
      next st@(bs :*: s :*: i) =
        case s of
          S (J a) N _ _             | U8.validate1 a ->
            Yield (unsafeChr8 a) es
          S (J a) (J b) N _         | U8.validate2 a b ->
            Yield (U8.chr2 a b) es
          S (J a) (J b) (J c) N     | U8.validate3 a b c ->
            Yield (U8.chr3 a b c) es
          S (J a) (J b) (J c) (J d) | U8.validate4 a b c d ->
            Yield (U8.chr4 a b c d) es
          _ -> consume st
         where es = bs :*: empty :*: i
      {-# INLINE consume #-}
      consume (bs@(Chunk ps rest) :*: s :*: i)
          | i >= B.length ps = consume (rest :*: s  :*: 0)
          | otherwise =
        case s of
          S N _ _ _ -> next (bs :*: S x N N N :*: i+1)
          S a N _ _ -> next (bs :*: S a x N N :*: i+1)
          S a b N _ -> next (bs :*: S a b x N :*: i+1)
          S a b c N -> next (bs :*: S a b c x :*: i+1)
          S (J a) b c d -> decodeError "streamUtf8" "UTF-8" onErr (Just a)
                           (bs :*: S b c d N :*: i+1)
          where x = J (B.unsafeIndex ps i)
      consume (Empty :*: S N _ _ _ :*: _) = Done
      consume st = decodeError "streamUtf8" "UTF-8" onErr Nothing st
{-# INLINE [0] streamUtf8 #-}

-- | /O(n)/ Convert a 'Stream' 'Word8' to a lazy 'ByteString'.
unstreamChunks :: Int -> Stream Word8 -> ByteString
unstreamChunks chunkSize (Stream next s0 len0) = chunk s0 (upperBound 4 len0)
  where chunk s1 len1 = unsafePerformIO $ do
          let len = min len1 chunkSize
          mallocByteString len >>= loop len 0 s1
          where
            loop !n !off !s fp = case next s of
                Done | off == 0 -> return Empty
                     | otherwise -> do
                      bs <- trimUp fp off
                      return $! Chunk bs Empty
                Skip s' -> loop n off s' fp
                Yield x s'
                    | off == chunkSize -> do
                      bs <- trimUp fp off
                      return (Chunk bs (chunk s (n - B.length bs)))
                    | off == n -> realloc fp n off s' x
                    | otherwise -> do
                      withForeignPtr fp $ \p -> pokeByteOff p off x
                      loop n (off+1) s' fp
            {-# NOINLINE realloc #-}
            realloc fp n off s x = do
              let n' = min (n+n) chunkSize
              fp' <- copy0 fp n n'
              withForeignPtr fp' $ \p -> pokeByteOff p off x
              loop n' (off+1) s fp'
            {-# NOINLINE trimUp #-}
            trimUp fp off = return $! B.PS fp 0 off
            copy0 :: ForeignPtr Word8 -> Int -> Int -> IO (ForeignPtr Word8)
            copy0 !src !srcLen !destLen = assert (srcLen <= destLen) $ do
                dest <- mallocByteString destLen
                withForeignPtr src  $ \src'  ->
                    withForeignPtr dest $ \dest' ->
                        memcpy dest' src' (fromIntegral srcLen)
                return dest

-- | /O(n)/ Convert a 'Stream' 'Word8' to a lazy 'ByteString'.
unstream :: Stream Word8 -> ByteString
unstream = unstreamChunks defaultChunkSize

decodeError :: forall s. String -> String -> OnDecodeError -> Maybe Word8
            -> s -> Step s Char
decodeError func kind onErr mb i =
    case onErr desc mb of
      Nothing -> Skip i
      Just c  -> Yield c i
    where desc = "Data.Text.Lazy.Encoding.Fusion." ++ func ++ ": Invalid " ++
                 kind ++ " stream"
