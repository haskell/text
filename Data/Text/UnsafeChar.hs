{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Text.UnsafeChar where

import GHC.Exts
import GHC.Word

unsafeChr :: Word16 -> Char
unsafeChr (W16# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr #-}

unsafeChr8 :: Word8 -> Char
unsafeChr8 (W8# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr8 #-}

unsafeChr32 :: Word32 -> Char
unsafeChr32 (W32# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr32 #-}
