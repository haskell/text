-- | This module provides quickcheck utilities, e.g. arbitrary and show
-- instances, and comparison functions, so we can focus on the actual properties
-- in the 'Tests.Properties' module.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.QuickCheckUtils
    ( BigInt(..)
    , NotEmpty(..)
    , Sqrt(..)
    , SpacyString(..)
    , SkewedBool(..)

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
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Text.Foreign (I8)
import Data.Text.Lazy.Builder.RealFloat (FPFormat(..))
import Data.Word (Word8, Word16)
import GHC.IO.Encoding.Types (TextEncoding(TextEncoding,textEncodingName))
import Test.QuickCheck (Arbitrary(..), arbitraryUnicodeChar, arbitraryBoundedEnum, getUnicodeString, arbitrarySizedIntegral, shrinkIntegral, Property, ioProperty, discard, counterexample, scale, (.&&.), NonEmptyList(..), forAll, getPositive, noShrinking)
import Test.QuickCheck.Gen (Gen, choose, chooseAny, elements, frequency, listOf, oneof, resize, sized)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
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

genWord8 :: Gen Word8
genWord8 = chooseAny

genWord16 :: Gen Word16
genWord16 = chooseAny

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
    arbitrary = fmap Sqrt $ sized $ \n -> resize (smallish n) arbitrary
        where
            smallish = round . (sqrt :: Double -> Double) . fromIntegral . abs
    shrink = map Sqrt . shrink . unSqrt

instance Arbitrary T.Text where
    arbitrary = do
        t <- (T.pack . getUnicodeString) `fmap` scale (* 2) arbitrary
        -- Generate chunks that start in the middle of their buffers.
        (\i -> T.drop i t) <$> choose (0, T.length t)
    shrink = map T.pack . shrink . T.unpack

instance Arbitrary TL.Text where
    arbitrary = (TL.fromChunks . map notEmpty . unSqrt) `fmap` arbitrary
    shrink = map TL.pack . shrink . TL.unpack

newtype BigInt = Big Integer
               deriving (Eq, Show)

instance Arbitrary BigInt where
    arbitrary = choose (1::Int,200) >>= \e -> Big <$> choose (10^(e-1),10^e)
    shrink (Big a) = [Big (a `div` 2^(l-e)) | e <- shrink l]
      where l = truncate (log (fromIntegral a) / log 2 :: Double) :: Integer

newtype NotEmpty a = NotEmpty { notEmpty :: a }
    deriving (Eq, Ord, Show)

instance Arbitrary (NotEmpty T.Text) where
    arbitrary   = fmap (NotEmpty . T.pack . getNonEmpty) arbitrary
    shrink      = fmap (NotEmpty . T.pack . getNonEmpty)
                . shrink . NonEmpty . T.unpack . notEmpty

instance Arbitrary (NotEmpty TL.Text) where
    arbitrary   = fmap (NotEmpty . TL.pack . getNonEmpty) arbitrary
    shrink      = fmap (NotEmpty . TL.pack . getNonEmpty)
                . shrink . NonEmpty . TL.unpack . notEmpty

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
eqP f g s w  = counterexample "orig" (f s =^= g t) .&&.
               counterexample "mini" (f s =^= g mini) .&&.
               counterexample "head" (f sa =^= g ta) .&&.
               counterexample "tail" (f sb =^= g tb)
    where t             = packS s
          mini          = packSChunkSize 10 s
          (sa,sb)       = splitAt m s
          (ta,tb)       = splitAtS m t
          l             = length s
          m | l == 0    = n
            | otherwise = n `mod` l
          n             = fromIntegral w

eqPSqrt :: (Eq a, Show a, Stringy s) =>
       (String -> a) -> (s -> a) -> Sqrt String -> Word8 -> Property
eqPSqrt f g s = eqP f g (unSqrt s)

instance Arbitrary FPFormat where
    arbitrary = arbitraryBoundedEnum

newtype Precision a = Precision (Maybe Int)
                    deriving (Eq, Show)

precision :: a -> Precision a -> Maybe Int
precision _ (Precision prec) = prec

arbitraryPrecision :: Int -> Gen (Precision a)
arbitraryPrecision maxDigits = Precision <$> do
  n <- choose (-1,maxDigits)
  return $ if n == -1
           then Nothing
           else Just n

instance Arbitrary (Precision Float) where
    arbitrary = arbitraryPrecision 11
    shrink    = map Precision . shrink . precision undefined

instance Arbitrary (Precision Double) where
    arbitrary = arbitraryPrecision 22
    shrink    = map Precision . shrink . precision undefined

#if !MIN_VERSION_QuickCheck(2,14,3)
instance Arbitrary IO.Newline where
    arbitrary = oneof [return IO.LF, return IO.CRLF]

instance Arbitrary IO.NewlineMode where
    arbitrary = IO.NewlineMode <$> arbitrary <*> arbitrary
#endif

instance Arbitrary IO.BufferMode where
    arbitrary = oneof [ return IO.NoBuffering,
                        return IO.LineBuffering,
                        return (IO.BlockBuffering Nothing),
                        (IO.BlockBuffering . Just . (+1) . fromIntegral) `fmap`
                        genWord16 ]

-- This test harness is complex!  What property are we checking?
--
-- Reading after writing a multi-line file should give the same
-- results as were written.
--
-- What do we vary while checking this property?
-- * The lines themselves, scrubbed to contain neither CR nor LF.  (By
--   working with a list of lines, we ensure that the data will
--   sometimes contain line endings.)
-- * Newline translation mode.
-- * Buffering.
write_read :: forall a b c.
  (Eq a, Show a, Show c, Arbitrary c)
  => ([b] -> a)
  -> ((Char -> Bool) -> b -> b)
  -> (IO.Handle -> a -> IO ())
  -> (IO.Handle -> IO a)
  -> (c -> [b])
  -> [TestTree]
write_read unline filt writer reader modData
  = encodings <&> \enc@TextEncoding {textEncodingName} -> testGroup textEncodingName
    [ testProperty "NoBuffering" $ propTest enc (pure IO.NoBuffering)
    , testProperty "LineBuffering" $ propTest enc (pure IO.LineBuffering)
    , testProperty "BlockBuffering" $ propTest enc blockBuffering
    ]
  where
  propTest :: TextEncoding -> Gen IO.BufferMode -> NoShrink IO.NewlineMode -> c -> Property
  propTest _   _ (NoShrink (IO.NewlineMode IO.LF IO.CRLF)) _ = discard
  propTest enc genBufferMode (NoShrink nl) d = forAll (NoShrink <$> genBufferMode) $ \(NoShrink mode) -> ioProperty $ withTempFile $ \_ h -> do
    let ts = modData d
        t = unline . map (filt (not . (`elem` "\r\n"))) $ ts
    IO.hSetEncoding h enc
    IO.hSetNewlineMode h nl
    IO.hSetBuffering h mode
    () <- writer h t
    IO.hSeek h IO.AbsoluteSeek 0
    r <- reader h
    let isEq = r == t
    seq isEq $ pure $ counterexample (show r ++ bool " /= " " == " isEq ++ show t) isEq

  encodings = [IO.utf8, IO.utf8_bom, IO.utf16, IO.utf16le, IO.utf16be, IO.utf32, IO.utf32le, IO.utf32be]

  blockBuffering :: Gen IO.BufferMode
  blockBuffering = IO.BlockBuffering <$> fmap (fmap $ min 4 . getPositive) arbitrary

newtype NoShrink a = NoShrink a deriving Show
instance Arbitrary a => Arbitrary (NoShrink a) where
  arbitrary = NoShrink <$> arbitrary

-- Generate various Unicode space characters with high probability
arbitrarySpacyChar :: Gen Char
arbitrarySpacyChar = oneof
  [ arbitraryUnicodeChar
  , elements $ filter isSpace [minBound..maxBound]
  ]

newtype SpacyString = SpacyString { getSpacyString :: String }
  deriving (Eq, Ord, Show, Read)

instance Arbitrary SpacyString where
  arbitrary = SpacyString `fmap` listOf arbitrarySpacyChar
  shrink (SpacyString xs) = SpacyString `fmap` shrink xs

newtype SkewedBool = Skewed { getSkewed :: Bool }
  deriving Show

instance Arbitrary SkewedBool where
  arbitrary = Skewed <$> frequency [(1, pure False), (5, pure True)]

(<&>) :: [a] -> (a -> b) -> [b]
(<&>) = flip fmap

