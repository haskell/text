{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- |
module Data.Text.Internal.Encoding where

import Control.Exception (evaluate, try)
import Control.Monad.ST (ST, runST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.Bifunctor (Bifunctor(first))
import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short.Internal as SBS
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException, strictDecode, lenientDecode)
import Data.Text.Internal (Text(..), empty)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Data.Text.Show as T (singleton)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word32, Word8)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (poke, peekByteOff)
import GHC.Exts (byteArrayContents#, unsafeCoerce#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(PlainPtr))
import Data.Text.Internal.ByteStringCompat (withBS)
import Data.Text.Internal.Encoding.Utf8
  (Utf8CodePointState, utf8StartState, updateUtf8State, isUtf8StateIsComplete)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Internal as B hiding (empty, append)
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder.Prim.Internal as BP
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Encoding.Fusion as E
import qualified Data.Text.Internal.Fusion as F
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

#ifdef SIMDUTF
import Foreign.C.Types (CInt(..))
#elif !MIN_VERSION_bytestring(0,11,2)
import qualified Data.ByteString.Unsafe as B
#endif

-- | State of decoding a 'ByteString' in UTF-8.
-- It consists of a value representing whether or
-- not the last byte is a complete code point, and on incompletion what
-- the 1 to 3 end bytes are that make up the incomplete code point.
data Utf8State = Utf8State
  { -- | Current UTF-8 code point state of the 'ByteString's
    -- that have been validated thus far.
    utf8CodePointState :: Utf8CodePointState
    -- | Get the incomplete UTF-8 code point of the 'ByteString's that
    -- have been validated thus far. The first byte of the 'Word32'
    -- indicates the number of bytes of the code point are available,
    -- and is followed by the bytes of the code point.
  , partialUtf8CodePoint :: PartialUtf8CodePoint
  }
  deriving (Eq, Ord, Show)

-- | This represtents the starting state of a UTF-8 validation check.
startUtf8State :: Utf8State 
startUtf8State = Utf8State utf8StartState partUtf8CPEmpty

-- | Prefix of a valid UTF-8 encoded code point.
-- This consists of a length (in bytes) between 1 and 3 stored in the most
-- significant byte, and the actual bytes in the rest of the word.
newtype PartialUtf8CodePoint = PartialUtf8CodePoint Word32
  deriving (Eq, Ord, Show)

-- | Empty prefix.
partUtf8CPEmpty :: PartialUtf8CodePoint
partUtf8CPEmpty = PartialUtf8CodePoint 0

-- | Length of the partial code point, stored in the most significant byte.
partUtf8CPLen :: PartialUtf8CodePoint -> Int
partUtf8CPLen (PartialUtf8CodePoint w) = fromIntegral $ w `shiftR` 24

-- | Get the @n@-th byte, assuming it is within bounds: @0 <= n < partUtf8CPLen c@.
partUtf8CPUnsafeIndex ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Int -> PartialUtf8CodePoint -> Word8
partUtf8CPUnsafeIndex n (PartialUtf8CodePoint w) = 
#if defined(ASSERTS)
  assert (0 <= n && n < partUtf8CPLen w) $
#endif
  fromIntegral $ w `shiftR` (16 - 8 * n)

partUtf8CPUnsafeAppend ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  PartialUtf8CodePoint -> ByteString -> PartialUtf8CodePoint
partUtf8CPUnsafeAppend c@(PartialUtf8CodePoint word) bs =
#if defined(ASSERTS)
  assert (lenc + lenbs <= 3) $
#endif
  PartialUtf8CodePoint $
    tryPush 0 $ tryPush 1 $ tryPush 2 $ word + (fromIntegral lenbs `shiftL` 24)
  where
    lenc = partUtf8CPLen c
    lenbs = B.length bs
    tryPush i w =
      if i < lenbs
      then w + (fromIntegral (B.index bs i) `shiftL` fromIntegral (16 - 8 * (lenc + i)))
      else w

#ifdef SIMDUTF
foreign import ccall unsafe "_hs_text_is_valid_utf8" c_is_valid_utf8
    :: Ptr Word8 -> CSize -> IO CInt
#endif

-- | Validate a 'ByteString' as a UTF-8-encoded text.
--
-- @validateUtf8Chunk chunk = (n, es)@
--
-- This function returns two values:
--
-- * The value 'n' indicates the longest prefix of the 'ByteString'
--   that is valid UTF-8-encoded data.
-- * The value 'es' indicates whether the 'ByteString'
--
--     * (@Left p@) contains an invalid code point and where the next
--       (potentially valid) code point begins, so that @p - n@ is the
--       number of invalid bytes, or
--     * (@Right s@) is valid, and all of the remaining bytes starting
--       at inbex 'n' are the beginning of an incomplete UTF-8 code
--       point, and 's' is the resulting 'Utf8State' value, which
--       can be used to validate against a following 'ByteString' with
--       'validateNextUtf8Chunk'.
validateUtf8Chunk ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> (Int, Either Int Utf8State)
validateUtf8Chunk bs@(B.length -> len)
#if defined(SIMDUTF) || MIN_VERSION_bytestring(0,11,2)
  | guessUtf8Boundary > 0 &&
    -- the rest of the bytestring valid utf-8 up to the boundary
    (
#ifdef SIMDUTF
      withBS bs $ \ fp _ -> unsafeDupablePerformIO $
        unsafeWithForeignPtr fp $ \ptr -> (/= 0) <$>
          c_is_valid_utf8 ptr (fromIntegral guessUtf8Boundary)
#else
      B.isValidUtf8 $ B.take guessUtf8Boundary bs
#endif
    ) = getEndState guessUtf8Boundary
    -- No
  | otherwise = getEndState 0
    where
      getEndState ndx = validateUtf8 ndx ndx utf8StartState
      w n word8 = len >= n && word8 <= (B.index bs $ len - n)
      guessUtf8Boundary
        | w 3 0xf0 = len - 3  -- third to last char starts a four-byte code point
        | w 2 0xe0 = len - 2  -- pre-last char starts a three-or-four-byte code point
        | w 1 0xc2 = len - 1  -- last char starts a two-(or more-)byte code point
        | otherwise = len
#else
  = validateUtf8 0 0 utf8StartState
    where
#endif
      validateUtf8 !ndx0 ndx s
        | ndx < len =
          let ndx' = ndx + 1 in
          case updateUtf8State (B.index bs ndx) s of
            Just s' ->
              validateUtf8 (
                if isUtf8StateIsComplete s'
                then ndx'
                else ndx0
              ) ndx' s'
            Nothing -> (ndx0, Left $ if ndx == ndx0 then ndx' else ndx)
        | otherwise = (ndx0, Right $ Utf8State s (partUtf8CPUnsafeAppend partUtf8CPEmpty $ B.drop ndx0 bs))

-- | Validate a 'ByteString' as a contiuation of UTF-8-encoded text.
--
-- @validateNextUtf8Chunk chunk s = (n, es)@
--
-- This function returns two values:
--
-- * The value 'n' indicates the end position of longest prefix of the
--   'ByteString' that is valid UTF-8-encoded data from the starting
--   state 's'. If 's' contains an incomplete code point, the input
--   'ByteString' is considered a continuation. As a result 'n' will be
--   negative if the code point is still incomplete or is proven to be
--   invalid.
--   
-- * The value 'es' indicates whether the 'ByteString'
--
--     * (@Left p@) contains an invalid code point and where the next
--       (potentially valid) code point begins, so that @p - n@ is the
--       number of invalid bytes, or
--     * (@Right s'@) is valid, and all of the remaining bytes starting
--       at index 'n' are the beginning of an incomplete UTF-8 code
--       point, and `s'` is the resulting 'Utf8State' value, which
--       can be used to validate against a following 'ByteString'.
validateNextUtf8Chunk ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Utf8State -> (Int, Either Int Utf8State)
validateNextUtf8Chunk bs@(B.length -> len) st@(Utf8State s lead)
  | len > 0 =
    let g pos s'
          -- first things first. let's try to get to the start of the next code point
          | isUtf8StateIsComplete s' =
            -- found the beginning of the next code point, hand this off to someone else
            case validateUtf8Chunk $ B.drop pos bs of
              (len', mS) -> (pos + len', first (+ pos) mS)
          -- code point is not complete yet
          -- walk the rest of the code point until error, complete, or no more data
          | pos < len =
            case updateUtf8State (B.index bs pos) s' of
              -- error
              Nothing -> (leadPos, Left pos)
              -- keep going
              Just s'' -> g (pos + 1) s''
          -- no more data
          | otherwise = (leadPos, Right $ Utf8State s' (partUtf8CPUnsafeAppend lead bs))
    in g 0 s
  | otherwise = (leadPos, Right st)
    where leadPos = -(partUtf8CPLen lead)

-- | Validated UTF-8 data to be converted into a 'Text' value.
data TextDataStack = TextDataStack
  { -- | Returns a list of 'Text' and UTF-8-valid 'ByteString' values.
    dataStack :: [Either Text (Either PartialUtf8CodePoint ByteString)]
    -- | Returns total number of UTF-8 valid bytes in the stack.
  , stackLen :: Int
  }
  deriving Show

-- | Empty stack
emptyStack :: TextDataStack
emptyStack = TextDataStack [] 0

-- | Push a text value onto the stack
pushText :: Text -> TextDataStack -> TextDataStack
pushText t@(Text _ _ tLen) tds@(TextDataStack stack sLen) =
  if tLen > 0
  then TextDataStack (Left t : stack) $ sLen + tLen
  else tds

copyFromStack :: [Either Text (Either PartialUtf8CodePoint ByteString)] -> Int -> A.MArray s -> ST s ()
copyFromStack (dat : dataStack') tLen dst =
  (case dat of
    Left (Text arr0 off utf8Len) -> do
      let dstOff = tLen - utf8Len
      A.copyI utf8Len dst dstOff arr0 off
      pure dstOff
    Right encoded ->
      case encoded of
        Left partial -> do
          let utf8Len = partUtf8CPLen partial
              dstOff = tLen - utf8Len
              g i | i < utf8Len = do
                    A.unsafeWrite dst (dstOff + i) (partUtf8CPUnsafeIndex i partial)
                    g (i + 1)
                  | otherwise = pure ()
          g dstOff
          pure utf8Len
        Right bs@(B.length -> utf8Len) -> do
          let dstOff = tLen - utf8Len
          withBS bs $ \ fp _ ->
            unsafeIOToST . unsafeWithForeignPtr fp $ \ src ->
              unsafeSTToIO $ A.copyFromPointer dst dstOff src utf8Len
          pure dstOff) >>= (\ tLen' -> copyFromStack dataStack' tLen' dst)
copyFromStack _ _ _ = pure ()
{-# INLINE copyFromStack #-}

-- | Create a 'Text' value from the contents of a 'TextDataStack'.
stackToText ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  TextDataStack -> Text
stackToText (TextDataStack stack sLen)
  | sLen > 0 = runST $
    do
      dst <- A.new sLen
      copyFromStack stack sLen dst
      arr <- A.unsafeFreeze dst
      pure $ Text arr 0 sLen
  | otherwise = empty

-- | Decode a 'ByteString' in the context of what has been already been decoded.
--
-- The 'ByteString' is validated against the 'Utf8State' using the rules
-- governing 'validateNextUtf8Chunk'. The longest valid UTF-8 prefix is added
-- to the input 'TextDataStack' which is returned with the end position of the
-- valid prefix, and either the resulting 'Utf8State'
-- (@Right Utf8State@) or the position of the of the first (potentially)
-- valid byte after the invalid bytes with remainder of the 'ByteString'
-- (@Left (Int, ByteString)@).
decodeNextUtf8Chunk ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString
  -> Utf8State
  -> TextDataStack
  -> ((Int, Either (Int, ByteString) Utf8State), TextDataStack)
decodeNextUtf8Chunk bs s tds@(TextDataStack stack sLen) =
  case validateNextUtf8Chunk bs s of
    (len, res) ->
      let stackedData'
            | len >= 0 =
              let partCP = partialUtf8CodePoint s
                  partLen = partUtf8CPLen partCP
                  stackedData@(TextDataStack stack' sLen') =
                    if partLen > 0
                    then TextDataStack (Right (Left partCP) : stack) $ sLen + partLen
                    else tds
              in
              if len > 0
              then TextDataStack (Right (Right $ B.take len bs) : stack') $ sLen' + len
              else stackedData
            | otherwise = tds
      in
      ( ( len
        , case res of
            Left pos -> Left (pos, B.drop pos bs)
            Right s' -> Right s'
        )
      , stackedData'
      )

-- | Decode a 'ByteString' against a start 'Utf8State' with an empty
-- 'TextDataStack'.
--
-- @decodeUtf8Chunk bs = 'decodeNextUtf8Chunk' bs 'startUtf8State' 'emptyStack'@
decodeUtf8Chunk :: ByteString -> ((Int, Either (Int, ByteString) Utf8State), TextDataStack)
decodeUtf8Chunk bs = decodeNextUtf8Chunk bs startUtf8State emptyStack

-- | Call an error handler with the give 'String' message for each byte
-- in given 'ByteString' and lead data in the given 'Utf8State'
-- value. The bytes are the positions from 'errStart' (inclusive) to
-- 'errEnd' (exclusive). Any substitute characters are pushed onto the
-- supplied 'TextDataStack' argument.
handleUtf8Err
  :: OnDecodeError
  -> String
  -> Int
  -> Int
  -> Utf8State
  -> ByteString
  -> TextDataStack
  -> TextDataStack
handleUtf8Err onErr errMsg errStart errEnd s bs tds =
  let h errPos tds'
        | errPos < errEnd =
          h (errPos + 1) $
            case onErr errMsg . Just $ B.index bs errPos of
              Just c -> pushText (T.singleton c) tds'
              Nothing -> tds'
        | otherwise = tds'
  in
  ( if errStart < 0
    then
      let partCP = partialUtf8CodePoint s
          partCPLen = partUtf8CPLen partCP
          g i tds'
            | i < partCPLen = g (i + 1) $
              case onErr errMsg (Just (partUtf8CPUnsafeIndex i partCP)) of
                Just c -> pushText (T.singleton c) tds'
                Nothing -> tds'
            | otherwise = h (partCPLen + errStart) tds'
      in
      g 0
    else h errStart
  ) tds

invalidUtf8Msg :: String
invalidUtf8Msg = "Data.Text.Internal.Encoding: Invalid UTF-8 stream"

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- Surrogate code points in replacement character returned by 'OnDecodeError'
-- will be automatically remapped to the replacement char @U+FFFD@.
decodeUtf8With ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> ByteString -> Text
decodeUtf8With onErr bs =
  let g bs' res =
        case res of
          ((len, eS), tds) ->
            let h msg pos s = handleUtf8Err onErr msg len pos s bs' tds in
            case eS of
              Left (pos, bs'') -> g bs'' . decodeNextUtf8Chunk bs'' startUtf8State $ h invalidUtf8Msg pos startUtf8State
              Right s -> stackToText $ h "Data.Text.Internal.Encoding: Incomplete UTF-8 code point" (B.length bs') s
  in
  g bs $ decodeUtf8Chunk bs
