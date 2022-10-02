{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Data.Text.Encoding
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts,
--               (c) 2008, 2009 Tom Harper
--               (c) 2021 Andrew Lelechenko
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : portable
--
-- Functions for converting 'Text' values to and from 'ByteString',
-- using several standard encodings.
--
-- To gain access to a much larger family of encodings, use the
-- <http://hackage.haskell.org/package/text-icu text-icu package>.

module Data.Text.Encoding
    (
    -- * Decoding ByteStrings to Text
    -- $strict

    -- ** Total Functions #total#
    -- $total
      decodeLatin1
    , decodeUtf8Lenient
    , decodeAsciiPrefix
    , Utf8DecodeState
    , startUtf8State
    , outAvailableUtf8Text
    , decodeNextUtf8Chunk
    , decodeUtf8Chunk
    , recoverFromUtf8Error

    -- *** Catchable failure
    , decodeUtf8'

    -- *** Controllable error handling
    , decodeUtf8With
    , decodeUtf16LEWith
    , decodeUtf16BEWith
    , decodeUtf32LEWith
    , decodeUtf32BEWith

    -- *** Stream oriented decoding
    -- $stream
    , streamDecodeUtf8With
    , Decoding(..)

    -- ** Partial Functions
    -- $partial
    , decodeASCII
    , decodeUtf8
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE

    -- *** Stream oriented decoding
    , streamDecodeUtf8

    -- * Encoding Text to ByteStrings
    , encodeUtf8
    , encodeUtf16LE
    , encodeUtf16BE
    , encodeUtf32LE
    , encodeUtf32BE

    -- * Encoding Text using ByteString Builders
    , encodeUtf8Builder
    , encodeUtf8BuilderEscaped
    ) where

import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)

import Control.Exception (evaluate, try)
import Control.Monad.ST (runST)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException, strictDecode, lenientDecode)
import Data.Text.Internal (Text(..), empty, append)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Data.Text.Show as T (singleton)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word8)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (poke, peekByteOff)
import GHC.Exts (byteArrayContents#, unsafeCoerce#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(PlainPtr))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Internal as B hiding (empty, append)
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder.Prim.Internal as BP
import Data.Text.Internal.Encoding.Utf8 (Utf8CodePointState, utf8StartState, updateUtf8State, isUtf8StateIsComplete)
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Encoding.Fusion as E
import qualified Data.Text.Internal.Fusion as F
import Data.Text.Internal.ByteStringCompat
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

#ifdef SIMDUTF
import Foreign.C.Types (CInt(..))
#elif !MIN_VERSION_bytestring(0,11,2)
import qualified Data.ByteString.Unsafe as B
#endif

-- $strict
--
-- All of the single-parameter functions for decoding bytestrings
-- encoded in one of the Unicode Transformation Formats (UTF) operate
-- in a /strict/ mode: each will throw an exception if given invalid
-- input.
--
-- Each function has a variant, whose name is suffixed with -'With',
-- that gives greater control over the handling of decoding errors.
-- For instance, 'decodeUtf8' will throw an exception, but
-- 'decodeUtf8With' allows the programmer to determine what to do on a
-- decoding error.

-- $total
--
-- These functions facilitate total decoding and should be preferred
-- over their partial counterparts.

-- $partial
--
-- These functions are partial and should only be used with great caution
-- (preferably not at all). See "Data.Text.Encoding#g:total" for better
-- solutions.

-- | Decode a 'ByteString' containing 7-bit ASCII
-- encoded text.
--
-- This is a partial function: it checks that input does not contain
-- anything except ASCII and copies buffer or throws an error otherwise.
--
decodeASCII :: ByteString -> Text
decodeASCII bs =
  case decodeAsciiPrefix bs of
    (_, Just errPos) -> error $ "decodeASCII: detected non-ASCII codepoint at " ++ show errPos
    (t, Nothing) -> t

-- | Decode a 'ByteString' containing Latin-1 (aka ISO-8859-1) encoded text.
--
-- 'decodeLatin1' is semantically equivalent to
--  @Data.Text.pack . Data.ByteString.Char8.unpack@
--
-- This is a total function. However, bear in mind that decoding Latin-1 (non-ASCII)
-- characters to UTf-8 requires actual work and is not just buffer copying.
--
decodeLatin1 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Text
decodeLatin1 bs = withBS bs $ \fp len -> runST $ do
  dst <- A.new (2 * len)
  let inner srcOff dstOff = if srcOff >= len then return dstOff else do
        asciiPrefixLen <- fmap cSizeToInt $ unsafeIOToST $ unsafeWithForeignPtr fp $ \src ->
          c_is_ascii (src `plusPtr` srcOff) (src `plusPtr` len)
        if asciiPrefixLen == 0
        then do
          byte <- unsafeIOToST $ unsafeWithForeignPtr fp $ \src -> peekByteOff src srcOff
          A.unsafeWrite dst dstOff (0xC0 + (byte `shiftR` 6))
          A.unsafeWrite dst (dstOff + 1) (0x80 + (byte .&. 0x3F))
          inner (srcOff + 1) (dstOff + 2)
        else do
          unsafeIOToST $ unsafeWithForeignPtr fp $ \src ->
            unsafeSTToIO $ A.copyFromPointer dst dstOff (src `plusPtr` srcOff) asciiPrefixLen
          inner (srcOff + asciiPrefixLen) (dstOff + asciiPrefixLen)

  actualLen <- inner 0 0
  dst' <- A.resizeM dst actualLen
  arr <- A.unsafeFreeze dst'
  return $ Text arr 0 actualLen

foreign import ccall unsafe "_hs_text_is_ascii" c_is_ascii
    :: Ptr Word8 -> Ptr Word8 -> IO CSize

-- | Decode a 'ByteString' containing ASCII.
--
-- This is a total function. The 'ByteString' is decoded until either
-- the end is reached or it errors with the first non-ASCII 'Word8' is
-- encountered. In either case the function will return what 'Text' was
-- decoded. On error, the index of the non-ASCII 'Word8' is also returned.
--
-- @since 2.0.2
decodeAsciiPrefix
  :: ByteString
  -> (Text, Maybe (Word8, Int))
decodeAsciiPrefix bs = if B.null bs
  then (empty, Nothing)
  else unsafeDupablePerformIO $ withBS bs $ \ fp len ->
    unsafeWithForeignPtr fp $ \src -> do
      asciiPrefixLen <- fmap fromIntegral . c_is_ascii src $ src `plusPtr` len
      let !prefix = if asciiPrefixLen == 0
            then empty
            else runST $ do
              dst <- A.new asciiPrefixLen
              A.copyFromPointer dst 0 src asciiPrefixLen
              arr <- A.unsafeFreeze dst
              pure $ Text arr 0 asciiPrefixLen
      let suffix = if asciiPrefixLen < len
            then Just (B.index bs asciiPrefixLen, asciiPrefixLen)
            else Nothing
      pure (prefix, suffix)

#ifdef SIMDUTF
foreign import ccall unsafe "_hs_text_is_valid_utf8" c_is_valid_utf8
    :: Ptr Word8 -> CSize -> IO CInt
#endif

-- | A value that represents the state of a UTF-8 decoding process potentionally
-- across multiple 'ByteString's.
--
-- @since 2.0.2
data Utf8DecodeState = Utf8DecodeState
  (Maybe (Utf8CodePointState, Int))
  [ByteString]
  Int
  [Either (Text, Int) (ByteString, Int, Int, Int)]
  Int
  deriving (Show)

-- | This represents the begining state of a UTF-8 decoding process.
--
-- @since 2.0.2
startUtf8State :: Utf8DecodeState
startUtf8State = Utf8DecodeState (Just (utf8StartState, 0)) [] 0 [] 0

-- | Takes whatever data has been decoded thus far and spits it out as a `Text`
-- value and a `Utf8DecodeState` value that no longer references the decoded
-- data. This function operates on error states, but does not clear the error.
-- (See 'recoverFromUtf8Error'.)
--
-- @since 2.0.2
outAvailableUtf8Text :: Utf8DecodeState -> (Text, Utf8DecodeState)
outAvailableUtf8Text (Utf8DecodeState mCpSt bss bs1Off dataStack tLen) =
  if tLen > 0
  then runST $ do
    dst <- A.new tLen
    mapM_ (\ dat ->
      case dat of
        Left ((Text arr0 off utf8Len), dstOff) -> A.copyI utf8Len dst dstOff arr0 off
        Right (bs, bsOff, utf8Len, dstOff) ->
          withBS bs $ \ fp _ ->
            unsafeIOToST . unsafeWithForeignPtr fp $ \ src ->
              unsafeSTToIO $ A.copyFromPointer dst dstOff (src `plusPtr` bsOff) utf8Len
      ) dataStack
    arr <- A.unsafeFreeze dst
    pure (Text arr 0 tLen, Utf8DecodeState mCpSt bss bs1Off [] 0)
  else (empty, Utf8DecodeState mCpSt bss bs1Off dataStack tLen)

getCodePointStateOrError :: Utf8DecodeState -> Either Int Int
getCodePointStateOrError (Utf8DecodeState mCpSt (bs1@(B.length -> len1) : bss') bs1Off _ _) =
  case mCpSt of
    Nothing ->
      let (lenInit, _) = foldr
            (\ bs@(B.length -> len') (lenInit', (lenN', _)) ->
              (lenInit' + lenN', (len', bs))) (0, (len1 - bs1Off, bs1)) bss'
      in
      Left (bs1Off - lenInit)
    Just (_, cpLen) -> Right cpLen
getCodePointStateOrError _ = Right 0

decodeUtf8Chunks :: Utf8DecodeState -> (Either Int Int, Utf8DecodeState)
decodeUtf8Chunks st@(Utf8DecodeState _ [] _ _ _) = (getCodePointStateOrError st, st)
decodeUtf8Chunks st@(Utf8DecodeState Nothing _ _ _ _) = (getCodePointStateOrError st, st)
decodeUtf8Chunks
  st@(Utf8DecodeState
    (Just (cpSt, cpPos))
    bss@(bs1@(B.length -> len1) : bss')
    bs1Off
    dataStack
    tLen
  )
  =
  {-
bs1Off = 0  cpPos = Δbs1Off
   |    bs1Off     |   boundary = Δbs1Off
   v       v   bsX v           v
  |. . bs1 . .|. .|. . .bsN. . .|
  ^-----------^   ^-------------^
      len1            lenN
          ^---^   ^-----------^
          len1_   isValidBS span
          ^-------^
          lenInit
          ^---------------------^
                len
  --}
  let len1_ = len1 - bs1Off -- the length of the trailing portion of the first bytesting that's to be evaluated.
      (lenInit, (lenN, bsN)) = foldr
        (\ bs@(B.length -> len') (lenInit', (lenN', _)) ->
          (lenInit' + lenN', (len', bs))) (0, (len1_, bs1)) bss'
      len = lenInit + lenN
  in
  if len == cpPos
  then (getCodePointStateOrError st, st)
  else
    let index i =
          if i < len1_
          then B.index bs1 (i + bs1Off)
          else
            let index' i' (bs@(B.length -> len0) : bss'') =
                  if i' < len0
                  then B.index bs i'
                  else index' (i' - len0) bss''
                index' i' _ = B.index bsN i'
            in
            index' (i - len1_) bss'
        guessUtf8Boundary
          | wi 3 0xf0 = Just $ len - 3  -- third to last char starts a four-byte code point
          | wi 2 0xe0 = Just $ len - 2  -- pre-last char starts a three-or-four-byte code point
          | wi 1 0xc2 = Just $ len - 1  -- last char starts a two-(or more-)byte code point
          | wc 4 0xf8 0xf0 ||           -- last four bytes are a four-byte code point
            wc 3 0xf0 0xe0 ||           -- last three bytes are a three-byte code point
            wc 2 0xe0 0xc0 ||           -- last two bytes are a two-byte code point
            w 1 (< 0x80) = Just len     -- last char is ASCII
          | otherwise = Nothing         -- no clue
          where
            w n test = len >= n && test (index $ len - n)
            wc n mask word8 = w n $ (word8 ==) . (mask .&.)
            wi n word8 = w n (>= word8)
        -- queue the available valid data, trim the fat, and leave a spot for the code point state/error.
        stackValidUtf8 wordCount mCps =
          let bs1Off' = bs1Off + wordCount
              (bs1Off''', bss'''', dataStack'') =
                if wordCount < len1_
                then (bs1Off', bss, Right (bs1, bs1Off, wordCount, tLen) : dataStack)
                else
                  let stackValidUtf8' wordCount' bs1Off'' tLen' bss''@(bs'@(B.length -> len') : bss''') dataStack' =
                        if wordCount' < len'
                        then (bs1Off'', bss'', Right (bs', 0, wordCount', tLen') : dataStack')
                        else stackValidUtf8' (wordCount' - len') (bs1Off'' - len') (tLen' + len') bss''' (Right (bs', 0, len', tLen') : dataStack')
                      stackValidUtf8' _ _ _ _ dataStack' = (0, [], dataStack')
                  in
                  stackValidUtf8' (wordCount - len1_) (bs1Off' - len1) (tLen + len1_) bss' (Right (bs1, bs1Off, len1_, tLen) : dataStack)
              st' = Utf8DecodeState mCps bss'''' bs1Off''' dataStack'' $ tLen + wordCount
          in
          (getCodePointStateOrError st', st')
        huntDownError off ndx cps =
          if ndx < len
          then
            case updateUtf8State (index ndx) cps of
              Just cps' ->
                let ndx' = ndx + 1 in
                huntDownError (
                  if isUtf8StateIsComplete cps'
                  then ndx'
                  else off
                ) ndx' cps'
              Nothing -> stackValidUtf8 off Nothing
          else stackValidUtf8 off $ Just (cps, ndx - off)
    in
    -- did we find the boundary?
    case guessUtf8Boundary of
      -- yes
      Just boundary ->
        -- are we before it?
        if cpPos < boundary
        -- yes: let's check this incomplete code point before checking the rest up to the boundary
        then
          let checkIncompleteCodePoint cpSt' cpPos'
                -- a complete code point
                | isUtf8StateIsComplete cpSt' =
                  let getEndState ndx cpSt''
                        | ndx < len =
                          case updateUtf8State (index ndx) cpSt'' of
                            Nothing -> Nothing
                            Just cpSt''' -> getEndState (ndx + 1) cpSt'''
                        | otherwise = Just (cpSt'', ndx - boundary)
                      soFarSoGood =
                        stackValidUtf8 boundary $ getEndState boundary cpSt'
                  in
                  -- are we at the boundary?
                  if boundary == cpPos'
                  -- yes: get the state of the last code point
                  then soFarSoGood
                  -- no:
                  else
                    -- are we before bsN?
                    if cpPos' < lenInit
                    -- yes
                    then
                      -- keep walking the data until we get to bsN or an error
                      case updateUtf8State (index cpPos') cpSt' of
                        Just cpSt'' -> checkIncompleteCodePoint cpSt'' (cpPos' + 1)
                        Nothing -> (Left (-lenInit), Utf8DecodeState Nothing bss bs1Off dataStack tLen)
                    -- no: we're in bsN
                    else let
                          off = (if lenInit > 0
                            then cpPos' - lenInit
                            else cpPos' + bs1Off)
                      in
                      -- is the rest of the bytestring valid utf-8 up to the boundary?
                      if (
#ifdef SIMDUTF
                          withBS bsN $ \ fp _ -> unsafeDupablePerformIO $
                            unsafeWithForeignPtr fp $ \ptr -> (/= 0) <$>
                              c_is_valid_utf8 (plusPtr ptr off) (fromIntegral $ boundary - cpPos')
#elif MIN_VERSION_bytestring(0,11,2)
                          B.isValidUtf8 . B.take (boundary - cpPos') $ B.drop off bsN
#else
                          let bLen = boundary - cpPos'
                              step ndx cps
                                | ndx < off + bLen =
                                  case updateUtf8State (B.unsafeIndex bsN ndx) cps of
                                  Just cps' -> step (ndx + 1) cps'
                                  Nothing -> False
                                | otherwise = isUtf8StateIsComplete cps
                          in
                          step off utf8StartState
#endif
                        )
                      -- Yes
                      then soFarSoGood
                      -- No
                      else huntDownError cpPos' cpPos' cpSt'
                -- We're mid code point
                | otherwise =
                  if cpPos' < len
                  then
                    -- try to complete the code point
                    case updateUtf8State (index cpPos') cpSt' of
                      Just cpSt'' -> checkIncompleteCodePoint cpSt'' (cpPos' + 1)
                      -- just enough additional data to find an error with the code point
                      Nothing -> (Left (-lenInit), Utf8DecodeState Nothing bss bs1Off dataStack tLen)
                  else
                    -- didn't get enough additional data to complete the code point
                    (Right cpPos', Utf8DecodeState (Just (cpSt', cpPos')) bss bs1Off dataStack tLen)
          in
          checkIncompleteCodePoint cpSt cpPos
        -- no, we're past the boundary
        else
          -- the code point is the only thing that (potentially) changes
          let getEndCodePointState cpPos' cpSt'
                | cpPos' < len =
                  case updateUtf8State (index cpPos') cpSt' of
                    Nothing -> Nothing
                    Just cpSt'' -> getEndCodePointState (cpPos' + 1) cpSt''
                | otherwise = Just (cpSt', cpPos' - boundary)
              mCpStLen = getEndCodePointState cpPos cpSt
          in
          ( case mCpStLen of
            Nothing -> Left (-lenInit)
            Just _ -> Right len
          , Utf8DecodeState mCpStLen bss bs1Off dataStack tLen
          )
      -- no: there's an error
      Nothing -> huntDownError 0 cpPos cpSt

-- | Decodes a UTF-8 'ByteString' in the context of what has already been
-- decoded which is represented by the 'Utf8DecodeState' value. Returned is the
-- new decode state and either ('Right') the number of 'Word8's that make up the
-- incomplete code point at the end of the input, or ('Left') the start position
-- of an invalid code point that was encountered. The position is relative to
-- the start of the input 'ByteString'.
--
-- If the previous 'ByteString' ended with an incomplete code point, the
-- beginning of the input data will be treated as a continuation of the code
-- point. NOTE: That in this case if the input causes the previous incomplete
-- code point to be invalid, the returned error ('Left') position value will be
-- negative.
--
-- If decoding the last 'ByteString' resulted in a error. The input is ignored,
-- and the state value is returned unchanged. Error states can be handled with
-- 'recoverFromUtf8Error'.
--
-- @since 2.0.2
decodeNextUtf8Chunk ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
 ByteString -> Utf8DecodeState -> (Either Int Int, Utf8DecodeState)
decodeNextUtf8Chunk _ st@(Utf8DecodeState Nothing _ _ _ _) = (getCodePointStateOrError st, st)
decodeNextUtf8Chunk bs@(B.length -> len) st@(Utf8DecodeState mCpSt bss bs1Off dataStack tLen)
  | len == 0 = (getCodePointStateOrError st, st)
  | otherwise = decodeUtf8Chunks $ Utf8DecodeState mCpSt (bss ++ [bs]) bs1Off dataStack tLen

-- | Decodes a 'ByteString' from a clean state.
--
-- @decodeUtf8Chunk bs = 'decodeNextUtf8Chunk' bs 'startUtf8State'@
--
-- @since 2.0.2
decodeUtf8Chunk :: ByteString -> (Either Int Int, Utf8DecodeState)
decodeUtf8Chunk = flip decodeNextUtf8Chunk startUtf8State

-- | If the 'Utf8DecodeState' value indicates an error state, the 'Word8' that
-- the state value point to is replaced with the input 'Text' value which may
-- be empty. Decoding resumes after the text is inserted and produces the result
-- described by 'decodeNextUtf8Chunk'.
--
-- If not in an error state, the 'Text' is inserted at the end of the data, but
-- before an incomplete code point at the end of the last input 'ByteString'.
--
-- @since 2.0.2
recoverFromUtf8Error ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
 Text -> Utf8DecodeState -> (Either Int Int, Utf8DecodeState)
recoverFromUtf8Error t@(Text _ _ utf8Len) (Utf8DecodeState mCpSt bss@((B.length -> len) : bss') bs1Off dataStack tLen) =
  let dammit mCpSt' bss'' bs1Off' = decodeUtf8Chunks . Utf8DecodeState mCpSt' bss'' bs1Off'
        ( if utf8Len > 0
          then Left (t, tLen) : dataStack
          else dataStack
        ) $ tLen + utf8Len
  in
  case mCpSt of
    Nothing ->
      let g = dammit (Just (utf8StartState, 0))
          bs1Off' = bs1Off + 1
      in
      if bs1Off' == len
      then g bss' 0
      else g bss bs1Off'
    Just _ -> dammit mCpSt bss bs1Off
recoverFromUtf8Error t@(Text _ _ utf8Len) (Utf8DecodeState _ _ _ dataStack tLen) =
  decodeUtf8Chunks $ Utf8DecodeState
    (Just (utf8StartState, 0))
    []
    0
    (if utf8Len > 0
      then Left (t, tLen) : dataStack
      else dataStack
    ) $ tLen + utf8Len

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- Surrogate code points in replacement character returned by 'OnDecodeError'
-- will be automatically remapped to the replacement char @U+FFFD@.
decodeUtf8With ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> ByteString -> Text
decodeUtf8With onErr bs
  | B.null undecoded = txt
  | otherwise = txt `append` (case onErr desc (Just (B.head undecoded)) of
    Nothing -> txt'
    Just c  -> T.singleton c `append` txt')
  where
    (txt, undecoded) = decodeUtf8With2 onErr mempty bs
    txt' = decodeUtf8With onErr (B.tail undecoded)
    desc = "Data.Text.Internal.Encoding: Invalid UTF-8 stream"

-- | Decode two consecutive bytestrings, returning Text and undecoded remainder.
decodeUtf8With2 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> ByteString -> ByteString -> (Text, ByteString)
decodeUtf8With2 onErr bs1@(B.length -> len1) bs2@(B.length -> len2) =
  let g res isSecondBs =
        case res of
          ((Left pos), st) ->
            g ( recoverFromUtf8Error
                ( case onErr "Data.Text.Internal.Encoding: Invalid UTF-8 code point" . Just $
                    if pos >= 0
                    then B.index (if isSecondBs then bs2 else bs1) pos
                    else B.index bs1 (len1 + pos) of
                  Just c -> T.singleton c
                  Nothing -> empty
                ) st
              ) isSecondBs
          ((Right cpLen), st) ->
            if isSecondBs
            then
              ( fst $ outAvailableUtf8Text st
              , if cpLen > len2
                then B.drop (len1 + len2 - cpLen) bs1 `B.append` bs2
                else B.drop (len2 - cpLen) bs2
              )
            else g (decodeNextUtf8Chunk bs2 st) True
  in
  g (decodeUtf8Chunk bs1) False

-- $stream
--
-- The 'streamDecodeUtf8' and 'streamDecodeUtf8With' functions accept
-- a 'ByteString' that represents a possibly incomplete input (e.g. a
-- packet from a network stream) that may not end on a UTF-8 boundary.
--
-- 1. The maximal prefix of 'Text' that could be decoded from the
--    given input.
--
-- 2. The suffix of the 'ByteString' that could not be decoded due to
--    insufficient input.
--
-- 3. A function that accepts another 'ByteString'.  That string will
--    be assumed to directly follow the string that was passed as
--    input to the original function, and it will in turn be decoded.
--
-- To help understand the use of these functions, consider the Unicode
-- string @\"hi &#9731;\"@. If encoded as UTF-8, this becomes @\"hi
-- \\xe2\\x98\\x83\"@; the final @\'&#9731;\'@ is encoded as 3 bytes.
--
-- Now suppose that we receive this encoded string as 3 packets that
-- are split up on untidy boundaries: @[\"hi \\xe2\", \"\\x98\",
-- \"\\x83\"]@. We cannot decode the entire Unicode string until we
-- have received all three packets, but we would like to make progress
-- as we receive each one.
--
-- @
-- ghci> let s0\@('Some' _ _ f0) = 'streamDecodeUtf8' \"hi \\xe2\"
-- ghci> s0
-- 'Some' \"hi \" \"\\xe2\" _
-- @
--
-- We use the continuation @f0@ to decode our second packet.
--
-- @
-- ghci> let s1\@('Some' _ _ f1) = f0 \"\\x98\"
-- ghci> s1
-- 'Some' \"\" \"\\xe2\\x98\"
-- @
--
-- We could not give @f0@ enough input to decode anything, so it
-- returned an empty string. Once we feed our second continuation @f1@
-- the last byte of input, it will make progress.
--
-- @
-- ghci> let s2\@('Some' _ _ f2) = f1 \"\\x83\"
-- ghci> s2
-- 'Some' \"\\x2603\" \"\" _
-- @
--
-- If given invalid input, an exception will be thrown by the function
-- or continuation where it is encountered.

-- | A stream oriented decoding result.
--
-- @since 1.0.0.0
data Decoding = Some !Text !ByteString (ByteString -> Decoding)

instance Show Decoding where
    showsPrec d (Some t bs _) = showParen (d > prec) $
                                showString "Some " . showsPrec prec' t .
                                showChar ' ' . showsPrec prec' bs .
                                showString " _"
      where prec = 10; prec' = prec + 1

-- | Decode, in a stream oriented way, a 'ByteString' containing UTF-8
-- encoded text that is known to be valid.
--
-- If the input contains any invalid UTF-8 data, an exception will be
-- thrown (either by this function or a continuation) that cannot be
-- caught in pure code.  For more control over the handling of invalid
-- data, use 'streamDecodeUtf8With'.
--
-- @since 1.0.0.0
streamDecodeUtf8 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Decoding
streamDecodeUtf8 = streamDecodeUtf8With strictDecode

-- | Decode, in a stream oriented way, a lazy 'ByteString' containing UTF-8
-- encoded text.
--
-- @since 1.0.0.0
streamDecodeUtf8With ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> ByteString -> Decoding
streamDecodeUtf8With onErr = go mempty
  where
    go bs1 bs2 = Some txt undecoded (go undecoded)
      where
        (txt, undecoded) = decodeUtf8With2 onErr bs1 bs2

-- | Decode a 'ByteString' containing UTF-8 encoded text that is known
-- to be valid.
--
-- If the input contains any invalid UTF-8 data, an exception will be
-- thrown that cannot be caught in pure code.  For more control over
-- the handling of invalid data, use 'decodeUtf8'' or
-- 'decodeUtf8With'.
--
-- This is a partial function: it checks that input is a well-formed
-- UTF-8 sequence and copies buffer or throws an error otherwise.
--
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With strictDecode
{-# INLINE[0] decodeUtf8 #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- If the input contains any invalid UTF-8 data, the relevant
-- exception will be returned, otherwise the decoded text.
decodeUtf8' ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Either UnicodeException Text
decodeUtf8' = unsafeDupablePerformIO . try . evaluate . decodeUtf8With strictDecode
{-# INLINE decodeUtf8' #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- Any invalid input bytes will be replaced with the Unicode replacement
-- character U+FFFD.
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

-- | Encode text to a ByteString 'B.Builder' using UTF-8 encoding.
--
-- @since 1.1.0.0
encodeUtf8Builder :: Text -> B.Builder
encodeUtf8Builder =
    -- manual eta-expansion to ensure inlining works as expected
    \txt -> B.builder (step txt)
  where
    step txt@(Text arr off len) !k br@(B.BufferRange op ope)
      -- Ensure that the common case is not recursive and therefore yields
      -- better code.
      | op' <= ope = do
          unsafeSTToIO $ A.copyToPointer arr off op len
          k (B.BufferRange op' ope)
      | otherwise = textCopyStep txt k br
      where
        op' = op `plusPtr` len
{-# INLINE encodeUtf8Builder #-}

textCopyStep :: Text -> B.BuildStep a -> B.BuildStep a
textCopyStep (Text arr off len) k =
    go off (off + len)
  where
    go !ip !ipe (B.BufferRange op ope)
      | inpRemaining <= outRemaining = do
          unsafeSTToIO $ A.copyToPointer arr ip op inpRemaining
          let !br = B.BufferRange (op `plusPtr` inpRemaining) ope
          k br
      | otherwise = do
          unsafeSTToIO $ A.copyToPointer arr ip op outRemaining
          let !ip' = ip + outRemaining
          return $ B.bufferFull 1 ope (go ip' ipe)
      where
        outRemaining = ope `minusPtr` op
        inpRemaining = ipe - ip

-- | Encode text using UTF-8 encoding and escape the ASCII characters using
-- a 'BP.BoundedPrim'.
--
-- Use this function is to implement efficient encoders for text-based formats
-- like JSON or HTML.
--
-- @since 1.1.0.0
{-# INLINE encodeUtf8BuilderEscaped #-}
-- TODO: Extend documentation with references to source code in @blaze-html@
-- or @aeson@ that uses this function.
encodeUtf8BuilderEscaped :: BP.BoundedPrim Word8 -> Text -> B.Builder
encodeUtf8BuilderEscaped be =
    -- manual eta-expansion to ensure inlining works as expected
    \txt -> B.builder (mkBuildstep txt)
  where
    bound = max 4 $ BP.sizeBound be

    mkBuildstep (Text arr off len) !k =
        outerLoop off
      where
        iend = off + len

        outerLoop !i0 !br@(B.BufferRange op0 ope)
          | i0 >= iend       = k br
          | outRemaining > 0 = goPartial (i0 + min outRemaining inpRemaining)
          -- TODO: Use a loop with an integrated bound's check if outRemaining
          -- is smaller than 8, as this will save on divisions.
          | otherwise        = return $ B.bufferFull bound op0 (outerLoop i0)
          where
            outRemaining = (ope `minusPtr` op0) `quot` bound
            inpRemaining = iend - i0

            goPartial !iendTmp = go i0 op0
              where
                go !i !op
                  | i < iendTmp = do
                    let w = A.unsafeIndex arr i
                    if w < 0x80
                      then BP.runB be w op >>= go (i + 1)
                      else poke op w >> go (i + 1) (op `plusPtr` 1)
                  | otherwise = outerLoop i (B.BufferRange op ope)

-- | Encode text using UTF-8 encoding.
encodeUtf8 :: Text -> ByteString
encodeUtf8 (Text arr off len)
  | len == 0  = B.empty
  -- It would be easier to use Data.ByteString.Short.fromShort and slice later,
  -- but this is undesirable when len is significantly smaller than length arr.
  | otherwise = unsafeDupablePerformIO $ do
    marr@(A.MutableByteArray mba) <- unsafeSTToIO $ A.newPinned len
    unsafeSTToIO $ A.copyI len marr 0 arr off
    let fp = ForeignPtr (byteArrayContents# (unsafeCoerce# mba))
                        (PlainPtr mba)
    pure $ B.fromForeignPtr fp 0 len

-- | Decode text from little endian UTF-16 encoding.
decodeUtf16LEWith :: OnDecodeError -> ByteString -> Text
decodeUtf16LEWith onErr bs = F.unstream (E.streamUtf16LE onErr bs)
{-# INLINE decodeUtf16LEWith #-}

-- | Decode text from little endian UTF-16 encoding.
--
-- If the input contains any invalid little endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16LEWith'.
decodeUtf16LE :: ByteString -> Text
decodeUtf16LE = decodeUtf16LEWith strictDecode
{-# INLINE decodeUtf16LE #-}

-- | Decode text from big endian UTF-16 encoding.
decodeUtf16BEWith :: OnDecodeError -> ByteString -> Text
decodeUtf16BEWith onErr bs = F.unstream (E.streamUtf16BE onErr bs)
{-# INLINE decodeUtf16BEWith #-}

-- | Decode text from big endian UTF-16 encoding.
--
-- If the input contains any invalid big endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16BEWith'.
decodeUtf16BE :: ByteString -> Text
decodeUtf16BE = decodeUtf16BEWith strictDecode
{-# INLINE decodeUtf16BE #-}

-- | Encode text using little endian UTF-16 encoding.
encodeUtf16LE :: Text -> ByteString
encodeUtf16LE txt = E.unstream (E.restreamUtf16LE (F.stream txt))
{-# INLINE encodeUtf16LE #-}

-- | Encode text using big endian UTF-16 encoding.
encodeUtf16BE :: Text -> ByteString
encodeUtf16BE txt = E.unstream (E.restreamUtf16BE (F.stream txt))
{-# INLINE encodeUtf16BE #-}

-- | Decode text from little endian UTF-32 encoding.
decodeUtf32LEWith :: OnDecodeError -> ByteString -> Text
decodeUtf32LEWith onErr bs = F.unstream (E.streamUtf32LE onErr bs)
{-# INLINE decodeUtf32LEWith #-}

-- | Decode text from little endian UTF-32 encoding.
--
-- If the input contains any invalid little endian UTF-32 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf32LEWith'.
decodeUtf32LE :: ByteString -> Text
decodeUtf32LE = decodeUtf32LEWith strictDecode
{-# INLINE decodeUtf32LE #-}

-- | Decode text from big endian UTF-32 encoding.
decodeUtf32BEWith :: OnDecodeError -> ByteString -> Text
decodeUtf32BEWith onErr bs = F.unstream (E.streamUtf32BE onErr bs)
{-# INLINE decodeUtf32BEWith #-}

-- | Decode text from big endian UTF-32 encoding.
--
-- If the input contains any invalid big endian UTF-32 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf32BEWith'.
decodeUtf32BE :: ByteString -> Text
decodeUtf32BE = decodeUtf32BEWith strictDecode
{-# INLINE decodeUtf32BE #-}

-- | Encode text using little endian UTF-32 encoding.
encodeUtf32LE :: Text -> ByteString
encodeUtf32LE txt = E.unstream (E.restreamUtf32LE (F.stream txt))
{-# INLINE encodeUtf32LE #-}

-- | Encode text using big endian UTF-32 encoding.
encodeUtf32BE :: Text -> ByteString
encodeUtf32BE txt = E.unstream (E.restreamUtf32BE (F.stream txt))
{-# INLINE encodeUtf32BE #-}

cSizeToInt :: CSize -> Int
cSizeToInt = fromIntegral
