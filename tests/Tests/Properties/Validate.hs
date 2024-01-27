{-# LANGUAGE CPP #-}
module Tests.Properties.Validate (testValidate) where

import Data.Array.Byte (ByteArray)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Short (toShort)
import Data.Either (isRight)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.Internal.Validate (isValidUtf8ByteString, isValidUtf8ByteArray)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck ((===), Gen, Property,
  testProperty, arbitrary, forAllShrink, oneof, shrink)
import Tests.QuickCheckUtils ()
#if MIN_VERSION_bytestring(0,12,0)
import Data.ByteString.Short (unShortByteString)
#else
#if MIN_VERSION_bytestring(0,11,1)
import Data.ByteString.Short (ShortByteString(SBS))
#else
import Data.ByteString.Short.Internal (ShortByteString(SBS))
#endif
import Data.Array.Byte (ByteArray(ByteArray))

unShortByteString :: ShortByteString -> ByteArray
unShortByteString (SBS ba) = ByteArray ba
#endif

testValidate :: TestTree
testValidate = testGroup "validate"
  [ testProperty "bytestring" $ forAllShrink genByteString shrink $ \bs ->
      isValidUtf8ByteString bs === isRight (decodeUtf8' bs)
  , testProperty "bytearray" $ forAllByteArray $ \ba off len bs ->
      isValidUtf8ByteArray ba off len === isRight (decodeUtf8' bs)
  ]

genByteString :: Gen ByteString
genByteString = oneof
  [ arbitrary
  , encodeUtf8 <$> arbitrary
  ]

-- | We want to test 'isValidUtf8ByteArray' with various offsets, so we insert a random
-- prefix and remember its length.
forAllByteArray :: (ByteArray -> Int -> Int -> ByteString -> Property) -> Property
forAllByteArray prop =
  forAllShrink genByteString shrink $ \mainSlice ->
  forAllShrink arbitrary shrink $ \prefix ->
  let bs2ba = unShortByteString . toShort in
  prop (bs2ba (prefix `B.append` mainSlice)) (B.length prefix) (B.length mainSlice) mainSlice
