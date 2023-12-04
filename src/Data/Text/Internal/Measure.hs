{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

#if defined(PURE_HASKELL)
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
#endif

#if !defined(PURE_HASKELL)
{-# LANGUAGE UnliftedFFITypes #-}
#endif

{-# OPTIONS_HADDOCK not-home #-}

-- | Implements 'measure_off', using efficient C routines by default.
module Data.Text.Internal.Measure
  ( measure_off
  )
where

import GHC.Exts

#if defined(PURE_HASKELL)
import GHC.Word
import Data.Text.Internal.Encoding.Utf8 (utf8LengthByLeader)
#endif

import Foreign.C.Types (CSize(..))
import System.Posix.Types (CSsize(..))

#if defined(PURE_HASKELL)

measure_off :: ByteArray# -> CSize -> CSize -> CSize -> CSsize
measure_off ba off len cnt = go 0 0
  where
    go !cc !i
      -- return the number of bytes for the first cnt codepoints,
      | cc == cnt = fromIntegral i
      -- return negated number of codepoints if there are fewer than cnt
      | i >= len  = negate (fromIntegral cc)
      | otherwise =
          let !(I# o) = fromIntegral (off+i)
              !b = indexWord8Array# ba o
          in go (cc+1) (i + fromIntegral (utf8LengthByLeader (W8# b)))

#else

-- | The input buffer (arr :: ByteArray#, off :: CSize, len :: CSize)
-- must specify a valid UTF-8 sequence, this condition is not checked.
foreign import ccall unsafe "_hs_text_measure_off" measure_off
    :: ByteArray# -> CSize -> CSize -> CSize -> CSsize

#endif
