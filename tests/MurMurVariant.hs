-- Simple murmuvariant
module MurMurVariant (
    MMV,
    showMMV,
    mmv,
    -- * Helpers
    mmvFromInteger,
    ) where

import           Data.Bits       (xor, shiftR)
import           Data.Word       (Word64)
import           Numeric         (showHex)

import qualified Data.ByteString as BS
import qualified Data.List       as L

newtype MMV = MMV Word64 deriving Eq

instance Show MMV where
    show = showMMV


-- | Show 'MMV' in human readable form
--
-- >>> showMMV 012345678
-- "0000000000bc614e"
--
-- >>> showMMV $ mmv $ BS.pack [0..127]
-- "5e13d02f1cd3879b"
--
showMMV :: MMV -> String
showMMV (MMV a) = pad a' where
    a' = showHex a ""
    pad s = replicate (16 - length s) '0' ++ s

mmv :: BS.ByteString -> MMV
mmv = MMV . L.foldl' f 0x9e3779b97f4a7c15 . BS.unpack where
    f acc w8 = mix64 (acc `xor` fromIntegral w8)

-- |
--
-- >>> showMMV $ mmvFromInteger 0x5e13d02f1cd3879b
-- "5e13d02f1cd3879b"
--
-- Note: the input is truncated:
--
-- >>> showMMV $ mmvFromInteger 0x123000005e13d02f1cd3879b
-- "5e13d02f1cd3879b"
--
-- Yet, negative numbers are not a problem...
--
-- >>> showMMV $ mmvFromInteger (-1)
-- "ffffffffffffffff"
--
mmvFromInteger :: Integer -> MMV
mmvFromInteger = MMV . fromInteger

mix64 :: Word64 -> Word64
mix64 z0 =
   -- MurmurHash3Mixer
    let z1 = shiftXorMultiply 33 0xff51afd7ed558ccd z0
        z2 = shiftXorMultiply 33 0xc4ceb9fe1a85ec53 z1
        z3 = shiftXor 33 z2
    in z3

shiftXor :: Int -> Word64 -> Word64
shiftXor n w = w `xor` (w `shiftR` n)

shiftXorMultiply :: Int -> Word64 -> Word64 -> Word64
shiftXorMultiply n k w = shiftXor n w * k
