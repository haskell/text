{-# LANGUAGE CPP, MagicHash, BangPatterns #-}

-- |
-- Module      : Data.Text.Internal.Encoding.Utf8
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--               (c) 2021 Andrew Lelechenko
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Basic UTF-8 validation and character manipulation.
module Data.Text.Internal.Encoding.Utf8
    ( utf8Length
    , utf8LengthByLeader
    -- Decomposition
    , ord2
    , ord3
    , ord4
    -- Construction
    , chr2
    , chr3
    , chr4
    -- * Validation
    , validate1
    , validate2
    , validate3
    , validate4
    -- * Naive decoding
    , DecoderState(..)
    , utf8AcceptState
    , utf8RejectState
    , updateDecoderState
    , DecoderResult(..)
    , CodePoint(..)
    , utf8DecodeStart
    , utf8DecodeContinue
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
import GHC.Stack (HasCallStack)
#endif
import Data.Bits (Bits(..), FiniteBits(..))
import Data.Char (ord, chr)
import GHC.Exts
import GHC.Word (Word8(..))

#if !MIN_VERSION_base(4,16,0)
-- harmless to import, except for warnings that it is unused.
import Data.Text.Internal.PrimCompat (word8ToWord#)
#endif

default(Int)

between :: Word8                -- ^ byte to check
        -> Word8                -- ^ lower bound
        -> Word8                -- ^ upper bound
        -> Bool
between x y z = x >= y && x <= z
{-# INLINE between #-}

-- This is a branchless version of
-- utf8Length c
--   | ord c < 0x80    = 1
--   | ord c < 0x800   = 2
--   | ord c < 0x10000 = 3
--   | otherwise       = 4
-- Implementation suggested by Alex Mason.

-- | @since 2.0
utf8Length :: Char -> Int
utf8Length (C# c) = I# ((1# +# geChar# c (chr# 0x80#)) +# (geChar# c (chr# 0x800#) +# geChar# c (chr# 0x10000#)))
{-# INLINE utf8Length #-}

-- This is a branchless version of
-- utf8LengthByLeader w
--   | w < 0x80  = 1
--   | w < 0xE0  = 2
--   | w < 0xF0  = 3
--   | otherwise = 4
--
-- c `xor` I# (c# <=# 0#) is a branchless equivalent of c `max` 1.
-- It is crucial to write c# <=# 0# and not c# ==# 0#, otherwise
-- GHC is tempted to "optimize" by introduction of branches.

-- | @since 2.0
utf8LengthByLeader :: Word8 -> Int
utf8LengthByLeader w = c `xor` I# (c# <=# 0#)
  where
    !c@(I# c#) = countLeadingZeros (complement w)
{-# INLINE utf8LengthByLeader #-}

ord2 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Char -> (Word8,Word8)
ord2 c =
#if defined(ASSERTS)
    assert (n >= 0x80 && n <= 0x07ff)
#endif
    (x1,x2)
    where
      n  = ord c
      x1 = intToWord8 $ (n `shiftR` 6) + 0xC0
      x2 = intToWord8 $ (n .&. 0x3F)   + 0x80
{-# INLINE ord2 #-}

ord3 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Char -> (Word8,Word8,Word8)
ord3 c =
#if defined(ASSERTS)
    assert (n >= 0x0800 && n <= 0xffff)
#endif
    (x1,x2,x3)
    where
      n  = ord c
      x1 = intToWord8 $ (n `shiftR` 12) + 0xE0
      x2 = intToWord8 $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x3 = intToWord8 $ (n .&. 0x3F) + 0x80
{-# INLINE ord3 #-}

ord4 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Char -> (Word8,Word8,Word8,Word8)
ord4 c =
#if defined(ASSERTS)
    assert (n >= 0x10000)
#endif
    (x1,x2,x3,x4)
    where
      n  = ord c
      x1 = intToWord8 $ (n `shiftR` 18) + 0xF0
      x2 = intToWord8 $ ((n `shiftR` 12) .&. 0x3F) + 0x80
      x3 = intToWord8 $ ((n `shiftR` 6) .&. 0x3F) + 0x80
      x4 = intToWord8 $ (n .&. 0x3F) + 0x80
{-# INLINE ord4 #-}

chr2 :: Word8 -> Word8 -> Char
chr2 (W8# x1#) (W8# x2#) = C# (chr# (z1# +# z2#))
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
      !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
      !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3 :: Word8 -> Word8 -> Word8 -> Char
chr3 (W8# x1#) (W8# x2#) (W8# x3#) = C# (chr# (z1# +# z2# +# z3#))
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
      !y3# = word2Int# (word8ToWord# x3#)
      !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
      !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
    C# (chr# (z1# +# z2# +# z3# +# z4#))
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
      !y3# = word2Int# (word8ToWord# x3#)
      !y4# = word2Int# (word8ToWord# x4#)
      !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
      !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
      !z4# = y4# -# 0x80#
{-# INLINE chr4 #-}

validate1 :: Word8 -> Bool
validate1 x1 = x1 <= 0x7F
{-# INLINE validate1 #-}

validate2 :: Word8 -> Word8 -> Bool
validate2 x1 x2 = between x1 0xC2 0xDF && between x2 0x80 0xBF
{-# INLINE validate2 #-}

validate3 :: Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate3 #-}
validate3 x1 x2 x3 = validate3_1 || validate3_2 || validate3_3 || validate3_4
  where
    validate3_1 = (x1 == 0xE0) &&
                  between x2 0xA0 0xBF &&
                  between x3 0x80 0xBF
    validate3_2 = between x1 0xE1 0xEC &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF
    validate3_3 = x1 == 0xED &&
                  between x2 0x80 0x9F &&
                  between x3 0x80 0xBF
    validate3_4 = between x1 0xEE 0xEF &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF

validate4 :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate4 #-}
validate4 x1 x2 x3 x4 = validate4_1 || validate4_2 || validate4_3
  where
    validate4_1 = x1 == 0xF0 &&
                  between x2 0x90 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_2 = between x1 0xF1 0xF3 &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_3 = x1 == 0xF4 &&
                  between x2 0x80 0x8F &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

-------------------------------------------------------------------------------
-- Naive UTF8 decoder.
-- See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for the explanation of the state machine.

newtype ByteClass = ByteClass Word8

byteToClass :: Word8 -> ByteClass
byteToClass n = ByteClass (W8# el#)
  where
    !(I# n#) = word8ToInt n
    el# = indexWord8OffAddr# table# n#

    table# :: Addr#
    table# = "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\SOH\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\a\b\b\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\STX\n\ETX\ETX\ETX\ETX\ETX\ETX\ETX\ETX\ETX\ETX\ETX\ETX\EOT\ETX\ETX\v\ACK\ACK\ACK\ENQ\b\b\b\b\b\b\b\b\b\b\b"#

newtype DecoderState = DecoderState Word8
  deriving (Eq, Show)

utf8AcceptState :: DecoderState
utf8AcceptState = DecoderState 0

utf8RejectState :: DecoderState
utf8RejectState = DecoderState 12

updateState :: ByteClass -> DecoderState -> DecoderState
updateState (ByteClass c) (DecoderState s) = DecoderState (W8# el#)
  where
    !(I# n#) = word8ToInt (c + s)
    el# = indexWord8OffAddr# table# n#

    table# :: Addr#
    table# = "\NUL\f\CAN$<`T\f\f\f0H\f\f\f\f\f\f\f\f\f\f\f\f\f\NUL\f\f\f\f\f\NUL\f\NUL\f\f\f\CAN\f\f\f\f\f\CAN\f\CAN\f\f\f\f\f\f\f\f\f\CAN\f\f\f\f\f\CAN\f\f\f\f\f\f\f\CAN\f\f\f\f\f\f\f\f\f$\f$\f\f\f$\f\f\f\f\f$\f$\f\f\f$\f\f\f\f\f\f\f\f\f\f"#

updateDecoderState :: Word8 -> DecoderState -> DecoderState
updateDecoderState b s = updateState (byteToClass b) s

newtype CodePoint = CodePoint Int

-- | @since 2.0
data DecoderResult
  = Accept !Char
  | Incomplete !DecoderState !CodePoint
  | Reject

-- | @since 2.0
utf8DecodeStart :: Word8 -> DecoderResult
utf8DecodeStart !w
  | st == utf8AcceptState = Accept (chr (word8ToInt w))
  | st == utf8RejectState = Reject
  | otherwise             = Incomplete st (CodePoint cp)
  where
    cl@(ByteClass cl') = byteToClass w
    st = updateState cl utf8AcceptState
    cp = word8ToInt $ (0xff `unsafeShiftR` word8ToInt cl') .&. w

-- | @since 2.0
utf8DecodeContinue :: Word8 -> DecoderState -> CodePoint -> DecoderResult
utf8DecodeContinue !w !st (CodePoint !cp)
  | st' == utf8AcceptState = Accept (chr cp')
  | st' == utf8RejectState = Reject
  | otherwise              = Incomplete st' (CodePoint cp')
  where
    cl  = byteToClass w
    st' = updateState cl st
    cp' = (cp `shiftL` 6) .|. word8ToInt (w .&. 0x3f)
