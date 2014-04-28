-- | This module provides quickcheck utilities, e.g. arbitrary and show
-- instances, and comparison functions, so we can focus on the actual properties
-- in the 'Tests.Properties' module.
--
{-# LANGUAGE CPP, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.QuickCheckUtils
    (
      genUnicode
    , genUnicodeWith
    , ascii
    , plane0
    , plane1
    , plane2
    , plane14
    , planes

    , unsquare
    , smallArbitrary

    , NotEmpty (..)

    , Small (..)
    , small

    , integralRandomR

    , DecodeErr (..)
    , genDecodeErr

    , Stringy (..)
    , eq
    , eqP

    , Encoding (..)

    , write_read
    ) where

import Control.Arrow (first, (***))
import Control.DeepSeq (NFData (..), deepseq)
import Control.Exception (bracket)
import Data.Bits ((.&.))
import Data.Char (chr)
import Data.String (IsString, fromString)
import Data.Text.Foreign (I16)
import Data.Word (Word8, Word16)
import Debug.Trace (trace)
import System.Random (Random (..), RandomGen)
import Test.QuickCheck hiding (Small (..), (.&.))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Tests.Utils
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Internal.Fusion as TF
import qualified Data.Text.Internal.Fusion.Common as TF
import qualified Data.Text.Internal.Lazy as TL
import qualified Data.Text.Internal.Lazy.Fusion as TLF
import qualified Data.Text.Lazy as TL
import qualified System.IO as IO

instance Random I16 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary I16 where
    arbitrary     = arbitrarySizedIntegral
    shrink        = shrinkIntegral

instance Arbitrary B.ByteString where
    arbitrary     = B.pack `fmap` arbitrary
    shrink        = map B.pack . shrink . B.unpack

#if !MIN_VERSION_base(4,4,0)
instance Random Word8 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)
#endif

genUnicode :: IsString a => Gen a
genUnicode = genUnicodeWith planes

genUnicodeWith :: IsString a => [Gen Int] -> Gen a
genUnicodeWith gens = fmap fromString string
  where
    string = sized $ \n ->
        do k <- choose (0,n)
           sequence [ char | _ <- [1..k] ]

    excluding :: [a -> Bool] -> Gen a -> Gen a
    excluding bad gen = loop
      where
        loop = do
          x <- gen
          if or (map ($ x) bad)
            then loop
            else return x

    reserved = [lowSurrogate, highSurrogate, noncharacter]
    lowSurrogate c = c >= 0xDC00 && c <= 0xDFFF
    highSurrogate c = c >= 0xD800 && c <= 0xDBFF
    noncharacter c = masked == 0xFFFE || masked == 0xFFFF
      where
        masked = c .&. 0xFFFF

    char = chr `fmap` excluding reserved (oneof gens)

ascii :: Gen Int
ascii = choose (0,0x7F)

plane0 :: Gen Int
plane0 = choose (0xF0, 0xFFFF)

plane1 :: Gen Int
plane1 = oneof [ choose (0x10000, 0x10FFF)
               , choose (0x11000, 0x11FFF)
               , choose (0x12000, 0x12FFF)
               , choose (0x13000, 0x13FFF)
               , choose (0x1D000, 0x1DFFF)
               , choose (0x1F000, 0x1FFFF)
               ]

plane2 :: Gen Int
plane2 = oneof [ choose (0x20000, 0x20FFF)
               , choose (0x21000, 0x21FFF)
               , choose (0x22000, 0x22FFF)
               , choose (0x23000, 0x23FFF)
               , choose (0x24000, 0x24FFF)
               , choose (0x25000, 0x25FFF)
               , choose (0x26000, 0x26FFF)
               , choose (0x27000, 0x27FFF)
               , choose (0x28000, 0x28FFF)
               , choose (0x29000, 0x29FFF)
               , choose (0x2A000, 0x2AFFF)
               , choose (0x2B000, 0x2BFFF)
               , choose (0x2F000, 0x2FFFF)
               ]

plane14 :: Gen Int
plane14 = choose (0xE0000, 0xE0FFF)

planes :: [Gen Int]
planes = [ascii, plane0, plane1, plane2, plane14]

-- For tests that have O(n^2) running times or input sizes, resize
-- their inputs to the square root of the originals.
unsquare :: (Arbitrary a, Show a, Testable b) => (a -> b) -> Property
unsquare = forAll smallArbitrary

smallArbitrary :: (Arbitrary a, Show a) => Gen a
smallArbitrary = sized $ \n -> resize (smallish n) arbitrary
  where smallish = round . (sqrt :: Double -> Double) . fromIntegral . abs

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary
    shrink = map T.pack . shrink . T.unpack

instance Arbitrary TL.Text where
    arbitrary = (TL.fromChunks . map notEmpty) `fmap` smallArbitrary
    shrink = map TL.pack . shrink . TL.unpack

newtype NotEmpty a = NotEmpty { notEmpty :: a }
    deriving (Eq, Ord)

instance Show a => Show (NotEmpty a) where
    show (NotEmpty a) = show a

instance Functor NotEmpty where
    fmap f (NotEmpty a) = NotEmpty (f a)

instance Arbitrary a => Arbitrary (NotEmpty [a]) where
    arbitrary   = sized (\n -> NotEmpty `fmap` (choose (1,n+1) >>= vector))
    shrink      = shrinkNotEmpty null

instance Arbitrary (NotEmpty T.Text) where
    arbitrary   = (fmap T.pack) `fmap` arbitrary
    shrink      = shrinkNotEmpty T.null

instance Arbitrary (NotEmpty TL.Text) where
    arbitrary   = (fmap TL.pack) `fmap` arbitrary
    shrink      = shrinkNotEmpty TL.null

instance Arbitrary (NotEmpty B.ByteString) where
    arbitrary   = (fmap B.pack) `fmap` arbitrary
    shrink      = shrinkNotEmpty B.null

shrinkNotEmpty :: Arbitrary a => (a -> Bool) -> NotEmpty a -> [NotEmpty a]
shrinkNotEmpty isNull (NotEmpty xs) =
  [ NotEmpty xs' | xs' <- shrink xs, not (isNull xs') ]

data Small = S0  | S1  | S2  | S3  | S4  | S5  | S6  | S7
           | S8  | S9  | S10 | S11 | S12 | S13 | S14 | S15
           | S16 | S17 | S18 | S19 | S20 | S21 | S22 | S23
           | S24 | S25 | S26 | S27 | S28 | S29 | S30 | S31
    deriving (Eq, Ord, Enum, Bounded)

small :: Integral a => Small -> a
small = fromIntegral . fromEnum

intf :: (Int -> Int -> Int) -> Small -> Small -> Small
intf f a b = toEnum ((fromEnum a `f` fromEnum b) `mod` 32)

instance Show Small where
    show = show . fromEnum

instance Read Small where
    readsPrec n = map (first toEnum) . readsPrec n

instance Num Small where
    fromInteger = toEnum . fromIntegral
    signum _ = 1
    abs = id
    (+) = intf (+)
    (-) = intf (-)
    (*) = intf (*)

instance Real Small where
    toRational = toRational . fromEnum

instance Integral Small where
    toInteger = toInteger . fromEnum
    quotRem a b = (toEnum x, toEnum y)
        where (x, y) = fromEnum a `quotRem` fromEnum b

instance Random Small where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary Small where
    arbitrary     = choose (minBound, maxBound)
    shrink        = shrinkIntegral

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,h) -> (fromIntegral x, h)

data DecodeErr = Lenient | Ignore | Strict | Replace
               deriving (Show, Eq)

genDecodeErr :: DecodeErr -> Gen T.OnDecodeError
genDecodeErr Lenient = return T.lenientDecode
genDecodeErr Ignore  = return T.ignore
genDecodeErr Strict  = return T.strictDecode
genDecodeErr Replace = arbitrary

instance Arbitrary DecodeErr where
    arbitrary = elements [Lenient, Ignore, Strict, Replace]

class Stringy s where
    packS    :: String -> s
    unpackS  :: s -> String
    splitAtS :: Int -> s -> (s,s)
    packSChunkSize :: Int -> String -> s
    packSChunkSize _ = packS

instance Stringy String where
    packS    = id
    unpackS  = id
    splitAtS = splitAt

instance Stringy (TF.Stream Char) where
    packS        = TF.streamList
    unpackS      = TF.unstreamList
    splitAtS n s = (TF.take n s, TF.drop n s)

instance Stringy T.Text where
    packS    = T.pack
    unpackS  = T.unpack
    splitAtS = T.splitAt

instance Stringy TL.Text where
    packSChunkSize k = TLF.unstreamChunks k . TF.streamList
    packS    = TL.pack
    unpackS  = TL.unpack
    splitAtS = ((TL.lazyInvariant *** TL.lazyInvariant) .) .
               TL.splitAt . fromIntegral

-- Do two functions give the same answer?
eq :: (Eq a, Show a) => (t -> a) -> (t -> a) -> t -> Bool
eq a b s  = a s =^= b s

-- What about with the RHS packed?
eqP :: (Eq a, Show a, Stringy s) =>
       (String -> a) -> (s -> a) -> String -> Word8 -> Bool
eqP f g s w  = eql "orig" (f s) (g t) &&
               eql "mini" (f s) (g mini) &&
               eql "head" (f sa) (g ta) &&
               eql "tail" (f sb) (g tb)
    where t             = packS s
          mini          = packSChunkSize 10 s
          (sa,sb)       = splitAt m s
          (ta,tb)       = splitAtS m t
          l             = length s
          m | l == 0    = n
            | otherwise = n `mod` l
          n             = fromIntegral w
          eql d a b
            | a =^= b   = True
            | otherwise = trace (d ++ ": " ++ show a ++ " /= " ++ show b) False

-- Work around lack of Show instance for TextEncoding.
data Encoding = E String IO.TextEncoding

instance Show Encoding where show (E n _) = "utf" ++ n

instance Arbitrary Encoding where
    arbitrary = oneof . map return $
      [ E "8" IO.utf8, E "8_bom" IO.utf8_bom, E "16" IO.utf16
      , E "16le" IO.utf16le, E "16be" IO.utf16be, E "32" IO.utf32
      , E "32le" IO.utf32le, E "32be" IO.utf32be
      ]

windowsNewlineMode :: IO.NewlineMode
windowsNewlineMode = IO.NewlineMode
    { IO.inputNL = IO.CRLF, IO.outputNL = IO.CRLF
    }

instance Arbitrary IO.NewlineMode where
    arbitrary = oneof . map return $
      [ IO.noNewlineTranslation, IO.universalNewlineMode, IO.nativeNewlineMode
      , windowsNewlineMode
      ]

instance Arbitrary IO.BufferMode where
    arbitrary = oneof [ return IO.NoBuffering,
                        return IO.LineBuffering,
                        return (IO.BlockBuffering Nothing),
                        (IO.BlockBuffering . Just . (+1) . fromIntegral) `fmap`
                        (arbitrary :: Gen Word16) ]

-- This test harness is complex!  What property are we checking?
--
-- Reading after writing a multi-line file should give the same
-- results as were written.
--
-- What do we vary while checking this property?
-- * The lines themselves, scrubbed to contain neither CR nor LF.  (By
--   working with a list of lines, we ensure that the data will
--   sometimes contain line endings.)
-- * Encoding.
-- * Newline translation mode.
-- * Buffering.
write_read :: (NFData a, Eq a)
           => ([b] -> a)
           -> ((Char -> Bool) -> a -> b)
           -> (IO.Handle -> a -> IO ())
           -> (IO.Handle -> IO a)
           -> Encoding
           -> IO.NewlineMode
           -> IO.BufferMode
           -> [a]
           -> Property
write_read unline filt writer reader (E _ _) nl buf ts =
    monadicIO $ assert . (==t) =<< run act
  where t = unline . map (filt (not . (`elem` "\r\n"))) $ ts
        act = withTempFile $ \path h -> do
                -- hSetEncoding h enc
                IO.hSetNewlineMode h nl
                IO.hSetBuffering h buf
                () <- writer h t
                IO.hClose h
                bracket (IO.openFile path IO.ReadMode) IO.hClose $ \h' -> do
                  -- hSetEncoding h' enc
                  IO.hSetNewlineMode h' nl
                  IO.hSetBuffering h' buf
                  r <- reader h'
                  r `deepseq` return r
