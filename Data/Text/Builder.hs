{-# LANGUAGE BangPatterns, Rank2Types, UnboxedTuples #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Text.Builder
-- Copyright   : Johan Tibell
-- License     : BSD3-style (see LICENSE)
-- 
-- Maintainer  : Johan Tibell <johan.tibell@gmail.com>
-- Stability   : experimental
-- Portability : portable to Hugs and GHC
--
-- Efficient construction of lazy texts.
--
-----------------------------------------------------------------------------

module Data.Text.Builder
   ( -- * The Builder type
     Builder
   , toLazyText

     -- * Constructing Builders
   , singleton
   , fromText
   , fromLazyText

     -- * Flushing the buffer state
   , flush
   ) where

import Control.Monad.ST
import Data.Bits ((.&.))
import Data.Char (ord)
import Data.Monoid (Monoid(..))
import Data.Text.Internal (Text(..))
import Data.Text.Lazy.Internal (defaultChunkSize)
import Data.Text.UnsafeShift (shiftR)
import Data.Word
import GHC.ST (ST(..))
import Prelude hiding (map, putChar)

import qualified Data.Text as S
import qualified Data.Text.Array as A
import qualified Data.Text.Lazy as L

------------------------------------------------------------------------

-- | A 'Builder' is an efficient way to build lazy 'L.Text's.  There
-- are several functions for constructing 'Builder's, but only one to
-- inspect them: to extract any data, you have to turn them into lazy
-- 'L.Text's using 'toLazyText'.
--
-- Internally, a 'Builder' constructs a lazy 'L.Text' by filling byte
-- arrays piece by piece.  As each buffer is filled, it is \'popped\'
-- off, to become a new chunk of the resulting lazy 'L.Text'.  All
-- this is hidden from the user of the 'Builder'.
newtype Builder = Builder {
     -- Invariant (from Data.Text.Lazy):
     --      The lists include no null Texts.
     runBuilder :: forall s. (Buffer s -> ST s [S.Text])
                -> Buffer s
                -> ST s [S.Text]
   }

instance Monoid Builder where
   mempty  = empty
   {-# INLINE mempty #-}
   mappend = append
   {-# INLINE mappend #-}

------------------------------------------------------------------------

-- | /O(1)./ The empty Builder, satisfying
--
--  * @'toLazyText' 'empty' = 'L.empty'@
--
empty :: Builder
empty = Builder id
{-# INLINE empty #-}

-- | /O(1)./ A Builder taking a single character, satisfying
--
--  * @'toLazyText' ('singleton' c) = 'L.singleton' c@
--
singleton :: Char -> Builder
singleton c = putChar c
{-# INLINE singleton #-}

------------------------------------------------------------------------

-- | /O(1)./ The concatenation of two Builders, an associative
-- operation with identity 'empty', satisfying
--
--  * @'toLazyText' ('append' x y) = 'L.append' ('toLazyText' x) ('toLazyText' y)@
--
append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder (f . g)
{-# INLINE append #-}

-- TODO: Experiment to find the right threshold.
copyLimit :: Int
copyLimit =  128                                 

-- This function attempts to merge small Texts instead of treating the
-- text as its own chunk.  We may not always want this.

-- | /O(1)./ A Builder taking a 'S.Text', satisfying
--
--  * @'toLazyText' ('fromText' t) = 'L.fromChunks' [t]@
--
fromText :: S.Text -> Builder
fromText t@(Text arr off l)
    | S.null t       = empty
    | l <= copyLimit = writeN l $ \marr o ->
          sequence_ [A.unsafeWrite marr (o+i) (A.unsafeIndex arr (off+i)) |
                     i <- [0..l-1]]
    | otherwise      = flush `append` mapBuilder (t :)
{-# INLINE fromText #-}

-- | /O(1)./ A Builder taking a lazy 'L.Text', satisfying
--
--  * @'toLazyText' ('fromLazyText' t) = t@
--
fromLazyText :: L.Text -> Builder
fromLazyText ts = flush `append` mapBuilder (L.toChunks ts ++)
{-# INLINE fromLazyText #-}

------------------------------------------------------------------------

-- Our internal buffer type
data Buffer s = Buffer {-# UNPACK #-} !(A.MArray s Word16)
                       {-# UNPACK #-} !Int  -- offset
                       {-# UNPACK #-} !Int  -- used units
                       {-# UNPACK #-} !Int  -- length left

------------------------------------------------------------------------

-- | /O(n)./ Extract a lazy 'L.T' from a 'Builder'.  The construction
-- work takes place if and when the relevant part of the lazy 'L.T' is
-- demanded.
--
toLazyText :: Builder -> L.Text
toLazyText m = L.fromChunks $ runST $ do
    buf <- newBuffer defaultChunkSize
    runBuilder (m `append` flush) (const (return [])) buf

-- | /O(1)./ Pop the 'S.Text' we have constructed so far, if any,
-- yielding a new chunk in the result lazy 'L.Text'.
flush :: Builder
flush = Builder $ \ k buf@(Buffer p o u l) ->
    if u == 0
        then k buf
        else do arr  <- A.unsafeFreeze p
                buf' <- inlineInterleaveST (k (Buffer p (o+u) 0 l))
                return (Text arr o u : buf')

------------------------------------------------------------------------

-- | Sequence an ST operation on the buffer
unsafeLiftST :: (forall s. Buffer s -> ST s (Buffer s)) -> Builder
unsafeLiftST f = Builder $ \k buf -> inlineInterleaveST $ do
    buf' <- f buf
    k buf'
{-# INLINE unsafeLiftST #-}

-- | Get the size of the buffer
withSize :: (Int -> Builder) -> Builder
withSize f = Builder $ \ k buf@(Buffer _ _ _ l) ->
    runBuilder (f l) k buf

-- | Map the resulting list of texts.
mapBuilder :: ([S.Text] -> [S.Text]) -> Builder
mapBuilder f = Builder $ \ k buf -> do txt <- k buf
                                       return (f txt)

------------------------------------------------------------------------

putChar :: Char -> Builder
putChar c
    | n < 0x10000 = writeN 1 $ \marr o -> A.unsafeWrite marr o (fromIntegral n)
    | otherwise   = writeN 2 $ \marr o -> do
          A.unsafeWrite marr o lo
          A.unsafeWrite marr (o+1) hi
  where n = ord c
        m = n - 0x10000
        lo = fromIntegral $ (m `shiftR` 10) + 0xD800
        hi = fromIntegral $ (m .&. 0x3FF) + 0xDC00
{-# INLINE putChar #-}

------------------------------------------------------------------------

-- | Ensure that there are at least @n@ many elements available.
ensureFree :: Int -> Builder
ensureFree n = n `seq` withSize $ \ l ->
    if n <= l
    then empty
    else flush `append` unsafeLiftST (const (newBuffer (max n defaultChunkSize)))
{-# INLINE ensureFree #-}

-- | Ensure that @n@ many elements are available, and then use @f@ to
-- write some elements into the memory.
writeN :: Int -> (forall s. A.MArray s Word16 -> Int -> ST s ()) -> Builder
writeN n f = ensureFree 1 `append` unsafeLiftST (writeNBuffer n f)
{-# INLINE writeN #-}

writeNBuffer :: Int -> (A.MArray s Word16 -> Int -> ST s ()) -> (Buffer s)
             -> ST s (Buffer s)
writeNBuffer n f (Buffer p o u l) = do
    f p (o+u)
    return (Buffer p o (u+n) (l-n))
{-# INLINE writeNBuffer #-}

newBuffer :: Int -> ST s (Buffer s)
newBuffer size = do
    arr <- A.unsafeNew size
    return $! Buffer arr 0 0 size
{-# INLINE newBuffer #-}

inlineInterleaveST :: ST s a -> ST s a
inlineInterleaveST (ST m) = ST $ \ s ->
    let r = case m s of (# _, res #) -> res in (# s, r #)
{-# INLINE inlineInterleaveST #-}
