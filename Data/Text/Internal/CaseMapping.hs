{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Text.Internal.CaseMapping
    (
      toUpper
    ) where

import Data.Text.Internal.Unsafe.Char (ord, unsafeChr)
import Data.Text.Internal.Unsafe (inlinePerformIO)
import Data.Word (Word16)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.C.Types (CSize)
import Data.Text.Internal.Fusion.Types
import Foreign.Storable (peek)
import qualified Data.Char as Char

search :: Ptr Word16 -> Int -> Word16 -> (Int -> IO a) -> IO a
search table len key cont = go 0 (len - 1)
  where
    go lo hi
      | hi < lo = cont (-1)
      | otherwise = do
        let mid = lo + (hi - lo) `quot` 2
        val <- peek (table `plusPtr` mid)
        case undefined of
          _ | val == key -> cont mid
            | val < key  -> go (mid + 1) hi
            | otherwise  -> go lo (mid - 1)

toUpper :: Char -> s -> Step (CC s) Char
toUpper c s
  | c > '\xffff' = Yield (Char.toUpper c) (CC s '\0' '\0')
  | otherwise    = inlinePerformIO $ do
    len <- fromIntegral `fmap` peek toUpperLen
    search toUpperKeys len (fromIntegral (ord c)) $ \idx -> do
      if (idx == -1)
        then return $ Yield (Char.toUpper c) (CC s '\0' '\0')
        else do
          let ptr = toUpperValues `plusPtr` (idx * 12)
          x <- unsafeChr `fmap` peek ptr
          y <- unsafeChr `fmap` peek (ptr `plusPtr` 2)
          z <- unsafeChr `fmap` peek (ptr `plusPtr` 4)
          return $ Yield x (CC s y z)


foreign import ccall "&_hs_text_to_upper_keys" toUpperKeys :: Ptr Word16
foreign import ccall "&_hs_text_to_upper_len" toUpperLen :: Ptr CSize
foreign import ccall "&_hs_text_to_upper_values" toUpperValues :: Ptr Word16
