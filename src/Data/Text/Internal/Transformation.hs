{-# LANGUAGE BangPatterns, CPP, MagicHash #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- |
-- Module      : Data.Text.Internal.Transformation
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module holds functions shared between the strict and lazy implementations of @Text@ transformations.

module Data.Text.Internal.Transformation
  ( mapNonEmpty
  , toCaseFoldNonEmpty
  , toLowerNonEmpty
  , toUpperNonEmpty
  , filter_
  ) where

import Prelude (Char, Bool(..), Int,
                Ord(..),
                Monad(..), pure,
                (+), (-), ($),
                not, return, otherwise)
import Data.Bits ((.&.), shiftR, shiftL)
import Control.Monad.ST (ST, runST)
import qualified Data.Text.Array as A
import Data.Text.Internal.Encoding.Utf8 (utf8LengthByLeader, chr2, chr3, chr4)
import Data.Text.Internal.Fusion.CaseMapping (foldMapping, lowerMapping, upperMapping)
import Data.Text.Internal (Text(..), safe)
import Data.Text.Internal.Unsafe.Char (unsafeWrite, unsafeChr8)
import qualified Prelude as P
import Data.Text.Unsafe (Iter(..), iterArray)
import Data.Word (Word8)
import qualified GHC.Exts as Exts
import GHC.Int (Int64(..))

-- | /O(n)/ 'map' @f@ @t@ is the 'Text' obtained by applying @f@ to
-- each element of @t@.
-- Assume that the @Text@ is non-empty. The returned @Text@ is guaranteed to be non-empty.
mapNonEmpty :: (Char -> Char) -> Text -> Text
mapNonEmpty f = go
  where
    go (Text src o l) = runST $ do
      marr <- A.new (l + 4)
      outer marr (l + 4) o 0
      where
        outer :: forall s. A.MArray s -> Int -> Int -> Int -> ST s Text
        outer !dst !dstLen = inner
          where
            inner !srcOff !dstOff
              | srcOff >= l + o = do
                A.shrinkM dst dstOff
                arr <- A.unsafeFreeze dst
                return (Text arr 0 dstOff)
              | dstOff + 4 > dstLen = do
                let !dstLen' = dstLen + (l + o) - srcOff + 4
                dst' <- A.resizeM dst dstLen'
                outer dst' dstLen' srcOff dstOff
              | otherwise = do
                let !(Iter c d) = iterArray src srcOff
                d' <- unsafeWrite dst dstOff (safe (f c))
                inner (srcOff + d) (dstOff + d')
{-# INLINE mapNonEmpty #-}

caseConvert :: (Word8 -> Word8) -> (Exts.Char# -> _ {- unboxed Int64 -}) -> Text -> Text
caseConvert ascii remap (Text src o l) = runST $ do
  -- Case conversion a single code point may produce up to 3 code-points,
  -- each up to 4 bytes, so 12 in total.
  dst <- A.new (l + 12)
  outer dst l o 0
  where
    outer :: forall s. A.MArray s -> Int -> Int -> Int -> ST s Text
    outer !dst !dstLen = inner
      where
        inner !srcOff !dstOff
          | srcOff >= o + l = do
            A.shrinkM dst dstOff
            arr <- A.unsafeFreeze dst
            return (Text arr 0 dstOff)
          | dstOff + 12 > dstLen = do
            -- Ensure to extend the buffer by at least 12 bytes.
            let !dstLen' = dstLen + max 12 (l + o - srcOff)
            dst' <- A.resizeM dst dstLen'
            outer dst' dstLen' srcOff dstOff
          -- If a character is to remain unchanged, no need to decode Char back into UTF8,
          -- just copy bytes from input.
          | otherwise = do
            let m0 = A.unsafeIndex src srcOff
                m1 = A.unsafeIndex src (srcOff + 1)
                m2 = A.unsafeIndex src (srcOff + 2)
                m3 = A.unsafeIndex src (srcOff + 3)
                !d = utf8LengthByLeader m0
            case d of
              1 -> do
                A.unsafeWrite dst dstOff (ascii m0)
                inner (srcOff + 1) (dstOff + 1)
              2 -> do
                let !(Exts.C# c) = chr2 m0 m1
                dstOff' <- case I64# (remap c) of
                  0 -> do
                    A.unsafeWrite dst dstOff m0
                    A.unsafeWrite dst (dstOff + 1) m1
                    pure $ dstOff + 2
                  i -> writeMapping i dstOff
                inner (srcOff + 2) dstOff'
              3 -> do
                let !(Exts.C# c) = chr3 m0 m1 m2
                dstOff' <- case I64# (remap c) of
                  0 -> do
                    A.unsafeWrite dst dstOff m0
                    A.unsafeWrite dst (dstOff + 1) m1
                    A.unsafeWrite dst (dstOff + 2) m2
                    pure $ dstOff + 3
                  i -> writeMapping i dstOff
                inner (srcOff + 3) dstOff'
              _ -> do
                let !(Exts.C# c) = chr4 m0 m1 m2 m3
                dstOff' <- case I64# (remap c) of
                  0 -> do
                    A.unsafeWrite dst dstOff m0
                    A.unsafeWrite dst (dstOff + 1) m1
                    A.unsafeWrite dst (dstOff + 2) m2
                    A.unsafeWrite dst (dstOff + 3) m3
                    pure $ dstOff + 4
                  i -> writeMapping i dstOff
                inner (srcOff + 4) dstOff'

        writeMapping :: Int64 -> Int -> ST s Int
        writeMapping 0 dstOff = pure dstOff
        writeMapping i dstOff = do
          let (ch, j) = chopOffChar i
          d <- unsafeWrite dst dstOff ch
          writeMapping j (dstOff + d)

        chopOffChar :: Int64 -> (Char, Int64)
        chopOffChar ab = (chr a, ab `shiftR` 21)
          where
            chr (Exts.I# n) = Exts.C# (Exts.chr# n)
            mask = (1 `shiftL` 21) - 1
            a = P.fromIntegral $ ab .&. mask
{-# INLINE caseConvert #-}


-- | /O(n)/ Convert a string to folded case.
-- Assume that the @Text@ is non-empty. The returned @Text@ is guaranteed to be non-empty.
toCaseFoldNonEmpty :: Text -> Text
toCaseFoldNonEmpty  = \xs -> caseConvert (\w -> if w - 65 <= 25 then w + 32 else w) foldMapping xs
{-# INLINE toCaseFoldNonEmpty #-}

-- | /O(n)/ Convert a string to lower case, using simple case
-- conversion.
-- Assume that the @Text@ is non-empty. The returned @Text@ is guaranteed to be non-empty.
toLowerNonEmpty :: Text -> Text
toLowerNonEmpty = \xs -> caseConvert (\w -> if w - 65 <= 25 then w + 32 else w) lowerMapping xs
{-# INLINE toLowerNonEmpty #-}

-- | /O(n)/ Convert a string to upper case, using simple case
-- conversion.
-- Assume that the @Text@ is non-empty. The returned @Text@ is guaranteed to be non-empty.
toUpperNonEmpty :: Text -> Text
toUpperNonEmpty = \xs -> caseConvert (\w -> if w - 97 <= 25 then w - 32 else w) upperMapping xs
{-# INLINE toUpperNonEmpty #-}

-- | /O(n)/ 'filter_', applied to a continuation, a predicate and a @Text@,
-- calls the continuation with the @Text@ containing only the characters satisfying the predicate.
filter_ :: forall a. (A.Array -> Int -> Int -> a) -> (Char -> Bool) -> Text -> a
filter_ mkText p = go
  where
    go (Text src o l) = runST $ do
      -- It's tempting to allocate l elements at once and avoid resizing.
      -- However, this can be unacceptable in scenarios where a huge array
      -- is filtered with a rare predicate, resulting in a much shorter buffer.
      let !dstLen = min l 64
      dst <- A.new dstLen
      outer dst dstLen o 0
      where
        outer :: forall s. A.MArray s -> Int -> Int -> Int -> ST s a
        outer !dst !dstLen = inner
          where
            inner !srcOff !dstOff
              | srcOff >= o + l = do
                A.shrinkM dst dstOff
                arr <- A.unsafeFreeze dst
                return $ mkText arr 0 dstOff
              | dstOff + 4 > dstLen = do
                -- Double size of the buffer, unless it becomes longer than
                -- source string. Ensure to extend it by least 4 bytes.
                let !dstLen' = dstLen + max 4 (min (l + o - srcOff) dstLen)
                dst' <- A.resizeM dst dstLen'
                outer dst' dstLen' srcOff dstOff
              -- In case of success, filter writes exactly the same character
              -- it just read (this is not a case for map, for example).
              -- We leverage this fact below: no need to decode Char back into UTF8,
              -- just copy bytes from input.
              | otherwise = do
                let m0 = A.unsafeIndex src srcOff
                    m1 = A.unsafeIndex src (srcOff + 1)
                    m2 = A.unsafeIndex src (srcOff + 2)
                    m3 = A.unsafeIndex src (srcOff + 3)
                    !d = utf8LengthByLeader m0
                case d of
                  1 -> do
                    let !c = unsafeChr8 m0
                    if not (p c) then inner (srcOff + 1) dstOff else do
                      A.unsafeWrite dst dstOff m0
                      inner (srcOff + 1) (dstOff + 1)
                  2 -> do
                    let !c = chr2 m0 m1
                    if not (p c) then inner (srcOff + 2) dstOff else do
                      A.unsafeWrite dst dstOff m0
                      A.unsafeWrite dst (dstOff + 1) m1
                      inner (srcOff + 2) (dstOff + 2)
                  3 -> do
                    let !c = chr3 m0 m1 m2
                    if not (p c) then inner (srcOff + 3) dstOff else do
                      A.unsafeWrite dst dstOff m0
                      A.unsafeWrite dst (dstOff + 1) m1
                      A.unsafeWrite dst (dstOff + 2) m2
                      inner (srcOff + 3) (dstOff + 3)
                  _ -> do
                    let !c = chr4 m0 m1 m2 m3
                    if not (p c) then inner (srcOff + 4) dstOff else do
                      A.unsafeWrite dst dstOff m0
                      A.unsafeWrite dst (dstOff + 1) m1
                      A.unsafeWrite dst (dstOff + 2) m2
                      A.unsafeWrite dst (dstOff + 3) m3
                      inner (srcOff + 4) (dstOff + 4)
{-# INLINE filter_ #-}
