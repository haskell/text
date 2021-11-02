-- | This module provides quickcheck utilities, e.g. arbitrary and show
-- instances, and comparison functions, so we can focus on the actual properties
-- in the 'Tests.Properties' module.
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.QuickCheckUtils
    ( BigInt(..)
    , NotEmpty(..)
    , Sqrt(..)
    , SpacyString(..)

    , Precision(..)
    , precision

    , DecodeErr(..)
    , genDecodeErr

    , Stringy(..)
    , unpack2
    , eq
    , eqP
    , eqPSqrt

    , write_read
    ) where

import Control.Arrow ((***))
import Control.DeepSeq (NFData (..), deepseq)
import Control.Exception (bracket)
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.Text.Foreign (I8)
import Data.Text.Lazy.Builder.RealFloat (FPFormat(..))
import Data.Word (Word8, Word16)
import Test.QuickCheck hiding (Fixed(..), Small (..), (.&.))
import Tests.Utils
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Internal.Fusion as TF
import qualified Data.Text.Internal.Fusion.Common as TF
import qualified Data.Text.Internal.Lazy as TL
import qualified Data.Text.Internal.Lazy.Fusion as TLF
import qualified Data.Text.Lazy as TL
import qualified System.IO as IO
import Control.Applicative (liftA2, liftA3)
import Data.Bits (shiftR, shiftL, countLeadingZeros, finiteBitSize)
import GHC.Num (integerLog2, integerLogBase)

genWord8 :: Gen Word8
genWord8 = chooseAny

instance Arbitrary I8 where
    arbitrary     = arbitrarySizedIntegral
    shrink        = shrinkIntegral

instance Arbitrary B.ByteString where
    arbitrary     = B.pack `fmap` listOf genWord8
    shrink        = map B.pack . shrink . B.unpack

instance Arbitrary BL.ByteString where
    arbitrary = oneof
      [ BL.fromChunks <$> arbitrary
      -- so that a single utf8 code point could appear split over up to 4 chunks
      , BL.fromChunks . map B.singleton <$> listOf genWord8
      -- so that a code point with 4 byte long utf8 representation
      -- could appear split over 3 non-singleton chunks
      , (\a b c -> BL.fromChunks [a, b, c])
        <$> arbitrary
        <*> ((\a b -> B.pack [a, b]) <$> genWord8 <*> genWord8)
        <*> arbitrary
      ]
    shrink xs = BL.fromChunks <$> shrink (BL.toChunks xs)

-- | For tests that have O(n^2) running times or input sizes, resize
-- their inputs to the square root of the originals.
newtype Sqrt a = Sqrt { unSqrt :: a }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Sqrt a) where
    arbitrary = coerce $ sized $ \n -> resize (smallish n) $ arbitrary @a
     where
      smallish = intSqrt . abs
      intSqrt :: Int -> Int
      intSqrt n =
        if n < 2
          then n
          else
            let b2 = shiftR (finiteBitSize n - countLeadingZeros n) 1 in
            shiftR (shiftL 1 b2 + shiftR n b2) 1
    shrink = coerce (shrink @a)

instance Arbitrary T.Text where
    arbitrary = T.pack <$> listOf arbitraryUnicodeChar -- without surrogates
    shrink = map T.pack . shrink . T.unpack

instance Arbitrary TL.Text where
    arbitrary = TL.fromChunks <$> coerce (arbitrary @(Sqrt [NotEmpty T.Text]))
    shrink = map TL.pack . shrink . TL.unpack

newtype BigInt = Big Integer
               deriving (Eq, Show)

instance Arbitrary BigInt where
    arbitrary = do
      e <- choose @Int (1,200)
      coerce $ choose @Integer (10^(e-1),10^e)

    shrink ba = [coerce (a `div` 2^(l-e)) | e <- shrink l]
     where
      a :: Integer
      a = coerce ba
      l :: Word
      l = integerLogBase 2 a

newtype NotEmpty a = NotEmpty { notEmpty :: a }
 deriving (Eq, Ord, Show)

toNotEmptyBy :: Functor m => ([Char] -> a) -> m (NonEmptyList Char) -> m (NotEmpty a)
toNotEmptyBy f = fmap (coerce f)

arbitraryNotEmptyBy :: ([Char] -> a) -> Gen (NotEmpty a)
arbitraryNotEmptyBy f = toNotEmptyBy f arbitrary

shrinkNotEmptyBy :: ([Char] -> a) -> (a -> [Char]) -> NotEmpty a -> [NotEmpty a]
shrinkNotEmptyBy g f =
  toNotEmptyBy g . shrink . coerce f

instance Arbitrary (NotEmpty T.Text) where
    arbitrary   = arbitraryNotEmptyBy T.pack
    shrink      = shrinkNotEmptyBy T.pack T.unpack

instance Arbitrary (NotEmpty TL.Text) where
    arbitrary   = arbitraryNotEmptyBy TL.pack
    shrink      = shrinkNotEmptyBy TL.pack TL.unpack


data DecodeErr = Lenient | Ignore | Strict | Replace
 deriving (Show, Eq, Bounded, Enum)

genDecodeErr :: DecodeErr -> Gen T.OnDecodeError
genDecodeErr Lenient = return T.lenientDecode
genDecodeErr Ignore  = return T.ignore
genDecodeErr Strict  = return T.strictDecode
genDecodeErr Replace = (\c _ _ -> c) <$> frequency
  [ (1, return Nothing)
  , (50, Just <$> arbitraryUnicodeChar)
  ]

instance Arbitrary DecodeErr where
    arbitrary = arbitraryBoundedEnum

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

unpack2 :: (Stringy s) => (s,s) -> (String,String)
unpack2 = unpackS *** unpackS

-- Do two functions give the same answer?
eq :: (Eq a, Show a) => (t -> a) -> (t -> a) -> t -> Property
eq a b s  = a s =^= b s

-- What about with the RHS packed?
eqP :: (Eq a, Show a, Stringy s) =>
       (String -> a) -> (s -> a) -> String -> Word8 -> Property
eqP f g s w  =
  testCounterExample "orig" s t .&&.
  testCounterExample "mini" s mini .&&.
  testCounterExample "head" sa ta .&&.
  testCounterExample "tail" sb tb
 where
  testCounterExample txt a b = counterexample txt $ f a =^= g b

  t       = packS s
  mini    = packSChunkSize 10 s
  (sa,sb) = splitAt m s
  (ta,tb) = splitAtS m t

  m       = if l == 0 then n else n `mod` l
   where
    l       = length s
    n       = fromIntegral w

eqPSqrt :: (Eq a, Show a, Stringy s) =>
       (String -> a) -> (s -> a) -> Sqrt String -> Word8 -> Property
eqPSqrt f g s = eqP f g $ coerce s

instance Arbitrary FPFormat where
    arbitrary = arbitraryBoundedEnum

newtype Precision a = Precision { unPrecision :: Maybe Int}
 deriving (Eq, Show)

--  Deprecated on 2021-10-05
precision :: a -> Precision a -> Maybe Int
precision _ = coerce
{-# DEPRECATED precision "Use @coerce@ or @unPrecision@ with types instead." #-}

arbitraryPrecision :: Int -> Gen (Precision a)
arbitraryPrecision maxDigits = do
  n <- choose (0,maxDigits)
  frequency
    [ (1, pure $ coerce $ Nothing @Int)
    , (n, pure $ coerce $ Just n)
    ]

instance Arbitrary (Precision Float) where
    arbitrary = arbitraryPrecision 11
    shrink    = coerce (shrink @(Maybe Int))

instance Arbitrary (Precision Double) where
    arbitrary = arbitraryPrecision 22
    shrink    = coerce (shrink @(Maybe Int))

instance Arbitrary IO.Newline where
    arbitrary = oneof [pure IO.LF, pure IO.CRLF]

instance Arbitrary IO.NewlineMode where
    arbitrary =
      liftA2 IO.NewlineMode
        arbitrary
        arbitrary

instance Arbitrary IO.BufferMode where
    arbitrary =
      oneof
        [ pure IO.NoBuffering
        , pure IO.LineBuffering
        , pure (IO.BlockBuffering Nothing)
        , IO.BlockBuffering . pure . succ . fromIntegral <$> arbitrary @Word16
        ]

-- This test harness is complex!  What property are we checking?
--
-- Reading after writing a multi-line file should give the same
-- results as were written.
--
-- What do we vary while checking this property?
--  * The lines themselves, scrubbed to contain neither CR nor LF.  (By
--    working with a list of lines, we ensure that the data will
--    sometimes contain line endings.)
--  * Newline translation mode.
--  * Buffering.
write_read :: (NFData a, Eq a, Show a)
           => ([b] -> a)
           -> ((Char -> Bool) -> a -> b)
           -> (IO.Handle -> a -> IO ())
           -> (IO.Handle -> IO a)
           -> IO.NewlineMode
           -> IO.BufferMode
           -> [a]
           -> Property
write_read _ _ _ _ (IO.NewlineMode IO.LF IO.CRLF) _ _ = discard
write_read unline filt writer reader nl buf ts = ioProperty $
    (===t) <$> act
  where
    t = unline . map (filt (`notElem` "\r\n")) $ ts

    act =
      withTempFile roundTrip
     where

      readBack h' = do
        IO.hSetNewlineMode h' nl
        IO.hSetBuffering h' buf
        r <- reader h'
        r `deepseq` pure r

      roundTrip path h = do
        IO.hSetNewlineMode h nl
        IO.hSetBuffering h buf
        () <- writer h t
        IO.hClose h

        IO.withFile path IO.ReadMode readBack

-- Generate various Unicode space characters with high probability
arbitrarySpacyChar :: Gen Char
arbitrarySpacyChar = oneof
  [ arbitraryUnicodeChar
  , elements $ filter isSpace [minBound..maxBound]
  ]

newtype SpacyString = SpacyString { getSpacyString :: String }
  deriving (Eq, Ord, Show, Read)

instance Arbitrary SpacyString where
  arbitrary = coerce $ listOf arbitrarySpacyChar
  shrink = coerce (shrink @[Char])
