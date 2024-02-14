{-# LANGUAGE BangPatterns, CPP, MagicHash, RankNTypes, UnboxedTuples, TypeFamilies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- |
-- Module      : Data.Text
-- Copyright   : (c) 2009, 2010, 2011, 2012 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts,
--               (c) 2008, 2009 Tom Harper
--               (c) 2021 Andrew Lelechenko
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- A time and space-efficient implementation of Unicode text.
-- Suitable for performance critical use, both in terms of large data
-- quantities and high speed.
--
-- /Note/: Read below the synopsis for important notes on the use of
-- this module.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.Text as T
--
-- To use an extended and very rich family of functions for working
-- with Unicode text (including normalization, regular expressions,
-- non-standard encodings, text breaking, and locales), see the
-- <http://hackage.haskell.org/package/text-icu text-icu package >.
--

module Data.Text
    (
    -- * Strict vs lazy types
    -- $strict

    -- * Acceptable data
    -- $replacement

    -- * Definition of character
    -- $character_definition

    -- * Fusion
    -- $fusion

    -- * Types
      Text
    , StrictText

    -- * Creation and elimination
    , pack
    , unpack
    , singleton
    , empty

    -- * Basic interface
    , cons
    , snoc
    , append
    , uncons
    , unsnoc
    , head
    , last
    , tail
    , init
    , null
    , length
    , compareLength

    -- * Transformations
    , map
    , intercalate
    , intersperse
    , transpose
    , reverse
    , replace

    -- ** Case conversion
    -- $case
    , toCaseFold
    , toLower
    , toUpper
    , toTitle

    -- ** Justification
    , justifyLeft
    , justifyRight
    , center

    -- * Folds
    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr'
    , foldr1

    -- ** Special folds
    , concat
    , concatMap
    , any
    , all
    , maximum
    , minimum
    , isAscii

    -- * Construction

    -- ** Scans
    , scanl
    , scanl1
    , scanr
    , scanr1

    -- ** Accumulating maps
    , mapAccumL
    , mapAccumR

    -- ** Generation and unfolding
    , replicate
    , unfoldr
    , unfoldrN

    -- * Substrings

    -- ** Breaking strings
    , take
    , takeEnd
    , drop
    , dropEnd
    , takeWhile
    , takeWhileEnd
    , dropWhile
    , dropWhileEnd
    , dropAround
    , strip
    , stripStart
    , stripEnd
    , splitAt
    , breakOn
    , breakOnEnd
    , break
    , span
    , spanM
    , spanEndM
    , group
    , groupBy
    , inits
    , tails

    -- ** Breaking into many substrings
    -- $split
    , splitOn
    , split
    , chunksOf

    -- ** Breaking into lines and words
    , lines
    --, lines'
    , words
    , unlines
    , unwords

    -- * Predicates
    , isPrefixOf
    , isSuffixOf
    , isInfixOf

    -- ** View patterns
    , stripPrefix
    , stripSuffix
    , commonPrefixes

    -- * Searching
    , filter
    , breakOnAll
    , find
    , elem
    , partition

    -- , findSubstring

    -- * Indexing
    -- $index
    , index
    , findIndex
    , count

    -- * Zipping
    , zip
    , zipWith

    -- -* Ordered text
    -- , sort

    -- * Low level operations
    , copy
    , unpackCString#
    , unpackCStringAscii#

    , measureOff
    ) where

import Prelude (Char, Bool(..), Int, Maybe(..), String,
                Eq, (==), (/=), Ord(..), Ordering(..), (++),
                Monad(..), pure, Read(..),
                (&&), (||), (+), (-), (.), ($), ($!), (>>),
                not, return, otherwise, quot)
import Control.DeepSeq (NFData(rnf))
#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.Bits ((.&.))
import qualified Data.Char as Char
import Data.Data (Data(gfoldl, toConstr, gunfold, dataTypeOf), constrIndex,
                  Constr, mkConstr, DataType, mkDataType, Fixity(Prefix))
import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import qualified Data.Text.Array as A
import qualified Data.List as L hiding (head, tail)
import Data.Binary (Binary(get, put))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Text.Internal.ArrayUtils (memchr)
import Data.Text.Internal.IsAscii (isAscii)
import Data.Text.Internal.Reverse (reverse)
import Data.Text.Internal.Measure (measure_off)
import Data.Text.Internal.Encoding.Utf8 (utf8Length, utf8LengthByLeader, chr3, ord2, ord3, ord4)
import qualified Data.Text.Internal.Fusion as S
import qualified Data.Text.Internal.Fusion.Common as S
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.Internal.Fusion (stream, reverseStream, unstream)
import Data.Text.Internal.Private (span_)
import Data.Text.Internal (Text(..), StrictText, empty, firstf, mul, safe, text, append, pack)
import Data.Text.Internal.Unsafe.Char (unsafeWrite)
import Data.Text.Show (singleton, unpack, unpackCString#, unpackCStringAscii#)
import qualified Prelude as P
import Data.Text.Unsafe (Iter(..), iter, iter_, lengthWord8, reverseIter,
                         reverseIter_, unsafeHead, unsafeTail, iterArray, reverseIterArray)
import Data.Text.Internal.Search (indices)
import Data.Text.Internal.Transformation (mapNonEmpty, toCaseFoldNonEmpty, toLowerNonEmpty, toUpperNonEmpty, filter_)
#if defined(__HADDOCK__)
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy as L
#endif
import Data.Word (Word8)
import Foreign.C.Types
import GHC.Base (eqInt, neInt, gtInt, geInt, ltInt, leInt)
import qualified GHC.Exts as Exts
import GHC.Int (Int8)
import GHC.Stack (HasCallStack)
import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH
import Text.Printf (PrintfArg, formatArg, formatString)
import System.Posix.Types (CSsize(..))

#if MIN_VERSION_template_haskell(2,16,0)
import Data.Text.Foreign (asForeignPtr)
import System.IO.Unsafe (unsafePerformIO)
#endif

-- $setup
-- >>> :set -package transformers
-- >>> import Control.Monad.Trans.State
-- >>> import Data.Text
-- >>> import qualified Data.Text as T
-- >>> :seti -XOverloadedStrings

-- $character_definition
--
-- This package uses the term /character/ to denote Unicode /code points/.
--
-- Note that this is not the same thing as a grapheme (e.g. a
-- composition of code points that form one visual symbol). For
-- instance, consider the grapheme \"&#x00e4;\". This symbol has two
-- Unicode representations: a single code-point representation
-- @U+00E4@ (the @LATIN SMALL LETTER A WITH DIAERESIS@ code point),
-- and a two code point representation @U+0061@ (the \"@A@\" code
-- point) and @U+0308@ (the @COMBINING DIAERESIS@ code point).

-- $strict
--
-- This package provides both strict and lazy 'Text' types.  The
-- strict type is provided by the "Data.Text" module, while the lazy
-- type is provided by the "Data.Text.Lazy" module. Internally, the
-- lazy @Text@ type consists of a list of strict chunks.
--
-- The strict 'Text' type requires that an entire string fit into
-- memory at once.  The lazy 'Data.Text.Lazy.Text' type is capable of
-- streaming strings that are larger than memory using a small memory
-- footprint.  In many cases, the overhead of chunked streaming makes
-- the lazy 'Data.Text.Lazy.Text' type slower than its strict
-- counterpart, but this is not always the case.  Sometimes, the time
-- complexity of a function in one module may be different from the
-- other, due to their differing internal structures.
--
-- Each module provides an almost identical API, with the main
-- difference being that the strict module uses 'Int' values for
-- lengths and counts, while the lazy module uses 'Data.Int.Int64'
-- lengths.

-- $replacement
--
-- A 'Text' value is a sequence of Unicode scalar values, as defined
-- in
-- <http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35 §3.9, definition D76 of the Unicode 5.2 standard >.
-- As such, a 'Text' cannot contain values in the range U+D800 to
-- U+DFFF inclusive. Haskell implementations admit all Unicode code
-- points
-- (<http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=13 §3.4, definition D10 >)
-- as 'Char' values, including code points from this invalid range.
-- This means that there are some 'Char' values
-- (corresponding to 'Data.Char.Surrogate' category) that are not valid
-- Unicode scalar values, and the functions in this module must handle
-- those cases.
--
-- Within this module, many functions construct a 'Text' from one or
-- more 'Char' values. Those functions will substitute 'Char' values
-- that are not valid Unicode scalar values with the replacement
-- character \"&#xfffd;\" (U+FFFD).  Functions that perform this
-- inspection and replacement are documented with the phrase
-- \"Performs replacement on invalid scalar values\". The functions replace
-- invalid scalar values, instead of dropping them, as a security
-- measure. For details, see
-- <http://unicode.org/reports/tr36/#Deletion_of_Noncharacters Unicode Technical Report 36, §3.5 >.)

-- $fusion
--
-- Starting from @text-1.3@ fusion is no longer implicit,
-- and pipelines of transformations usually allocate intermediate 'Text' values.
-- Users, who observe significant changes to performances,
-- are encouraged to use fusion framework explicitly, employing
-- "Data.Text.Internal.Fusion" and "Data.Text.Internal.Fusion.Common".

instance Eq Text where
    Text arrA offA lenA == Text arrB offB lenB
        | lenA == lenB = A.equal arrA offA arrB offB lenA
        | otherwise    = False
    {-# INLINE (==) #-}

instance Ord Text where
    compare = compareText

instance Read Text where
    readsPrec p str = [(pack x,y) | (x,y) <- readsPrec p str]

-- | @since 1.2.2.0
instance Semigroup Text where
    (<>) = append

instance Monoid Text where
    mempty  = empty
    mappend = (<>)
    mconcat = concat

-- | Performs replacement on invalid scalar values:
--
-- >>> :set -XOverloadedStrings
-- >>> "\55555" :: Text
-- "\65533"
instance IsString Text where
    fromString = pack

-- | Performs replacement on invalid scalar values:
--
-- >>> :set -XOverloadedLists
-- >>> ['\55555'] :: Text
-- "\65533"
--
-- @since 1.2.0.0
instance Exts.IsList Text where
    type Item Text = Char
    fromList       = pack
    toList         = unpack

instance NFData Text where rnf !_ = ()

-- | @since 1.2.1.0
instance Binary Text where
    put t = put (encodeUtf8 t)
    get   = do
      bs <- get
      case decodeUtf8' bs of
        P.Left exn -> P.fail (P.show exn)
        P.Right a -> P.return a

-- | This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.
--
-- This instance was created by copying the updated behavior of
-- @"Data.Set".@'Data.Set.Set' and @"Data.Map".@'Data.Map.Map'. If you
-- feel a mistake has been made, please feel free to submit
-- improvements.
--
-- The original discussion is archived here:
-- <https://mail.haskell.org/pipermail/haskell-cafe/2010-January/072379.html could we get a Data instance for Data.Text.Text? >
--
-- The followup discussion that changed the behavior of 'Data.Set.Set'
-- and 'Data.Map.Map' is archived here:
-- <https://mail.haskell.org/pipermail/libraries/2012-August/018366.html Proposal: Allow gunfold for Data.Map, ... >

instance Data Text where
  gfoldl f z txt = z pack `f` (unpack txt)
  toConstr _ = packConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z pack)
    _ -> P.error "gunfold"
  dataTypeOf _ = textDataType

-- | @since 1.2.4.0
instance TH.Lift Text where
#if MIN_VERSION_template_haskell(2,16,0)
  lift txt = do
    let (ptr, len) = unsafePerformIO $ asForeignPtr txt
    case len of
        0 -> TH.varE 'empty
        _ ->
          let
            bytesQ = TH.litE . TH.bytesPrimL $ TH.mkBytes ptr 0 (P.fromIntegral len)
            lenQ = liftInt (P.fromIntegral len)
            liftInt n = (TH.appE (TH.conE 'Exts.I#) (TH.litE (TH.IntPrimL n)))
          in TH.varE 'unpackCStringLen# `TH.appE` bytesQ `TH.appE` lenQ
#else
  lift = TH.appE (TH.varE 'pack) . TH.stringE . unpack
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

#if MIN_VERSION_template_haskell(2,16,0)
unpackCStringLen# :: Exts.Addr# -> Int -> Text
unpackCStringLen# addr# l = Text ba 0 l
  where
    ba = runST $ do
      marr <- A.new l
      A.copyFromPointer marr 0 (Exts.Ptr addr#) l
      A.unsafeFreeze marr
{-# NOINLINE unpackCStringLen# #-} -- set as NOINLINE to avoid generated code bloat
#endif

-- | @since 1.2.2.0
instance PrintfArg Text where
  formatArg txt = formatString $ unpack txt

packConstr :: Constr
packConstr = mkConstr textDataType "pack" [] Prefix

textDataType :: DataType
textDataType = mkDataType "Data.Text.Text" [packConstr]

-- | /O(n)/ Compare two 'Text' values lexicographically.
compareText :: Text -> Text -> Ordering
compareText (Text arrA offA lenA) (Text arrB offB lenB) =
    A.compare arrA offA arrB offB (min lenA lenB) <> compare lenA lenB
-- This is not a mistake: on contrary to UTF-16 (https://github.com/haskell/text/pull/208),
-- lexicographic ordering of UTF-8 encoded strings matches lexicographic ordering
-- of underlying bytearrays, no decoding is needed.

-- -----------------------------------------------------------------------------
-- * Basic functions

-- | /O(n)/ Adds a character to the front of a 'Text'.  This function
-- is more costly than its 'List' counterpart because it requires
-- copying a new array.  Performs replacement on
-- invalid scalar values.
cons :: Char -> Text -> Text
cons c = unstream . S.cons (safe c) . stream
{-# INLINE [1] cons #-}

infixr 5 `cons`

-- | /O(n)/ Adds a character to the end of a 'Text'.  This copies the
-- entire array in the process.
-- Performs replacement on invalid scalar values.
snoc :: Text -> Char -> Text
snoc t c = unstream (S.snoc (stream t) (safe c))
{-# INLINE snoc #-}

-- | /O(1)/ Returns the first character of a 'Text', which must be
-- non-empty. This is a partial function, consider using 'uncons' instead.
head :: HasCallStack => Text -> Char
head t = S.head (stream t)
{-# INLINE head #-}

-- | /O(1)/ Returns the first character and rest of a 'Text', or
-- 'Nothing' if empty.
uncons :: Text -> Maybe (Char, Text)
uncons t@(Text arr off len)
    | len <= 0  = Nothing
    | otherwise = Just $ let !(Iter c d) = iter t 0
                         in (c, text arr (off+d) (len-d))
{-# INLINE [1] uncons #-}

-- | /O(1)/ Returns the last character of a 'Text', which must be
-- non-empty. This is a partial function, consider using 'unsnoc' instead.
last :: HasCallStack => Text -> Char
last t@(Text _ _ len)
    | null t = emptyError "last"
    | otherwise = let Iter c _ = reverseIter t (len - 1) in c
{-# INLINE [1] last #-}

-- | /O(1)/ Returns all characters after the head of a 'Text', which
-- must be non-empty. This is a partial function, consider using 'uncons' instead.
tail :: HasCallStack => Text -> Text
tail t@(Text arr off len)
    | null t = emptyError "tail"
    | otherwise = text arr (off+d) (len-d)
    where d = iter_ t 0
{-# INLINE [1] tail #-}

-- | /O(1)/ Returns all but the last character of a 'Text', which must
-- be non-empty. This is a partial function, consider using 'unsnoc' instead.
init :: HasCallStack => Text -> Text
init t@(Text arr off len)
    | null t = emptyError "init"
    | otherwise = text arr off (len + reverseIter_ t (len - 1))
{-# INLINE [1] init #-}

-- | /O(1)/ Returns all but the last character and the last character of a
-- 'Text', or 'Nothing' if empty.
--
-- @since 1.2.3.0
unsnoc :: Text -> Maybe (Text, Char)
unsnoc t@(Text arr off len)
    | null t = Nothing
    | otherwise = Just (text arr off (len + d), c)
        where
            Iter c d = reverseIter t (len - 1)
{-# INLINE [1] unsnoc #-}

-- | /O(1)/ Tests whether a 'Text' is empty or not.
null :: Text -> Bool
null (Text _arr _off len) =
#if defined(ASSERTS)
    assert (len >= 0) $
#endif
    len <= 0
{-# INLINE [1] null #-}

{-# RULES 
 "TEXT null/empty -> True" null empty = True
#-}

-- | /O(1)/ Tests whether a 'Text' contains exactly one character.
isSingleton :: Text -> Bool
isSingleton = S.isSingleton . stream
{-# INLINE isSingleton #-}

-- | /O(n)/ Returns the number of characters in a 'Text'.
length ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> Int
length = P.negate . measureOff P.maxBound
{-# INLINE [1] length #-}
-- length needs to be phased after the compareN/length rules otherwise
-- it may inline before the rules have an opportunity to fire.

{-# RULES
"TEXT length/filter -> S.length/S.filter" forall p t.
    length (filter p t) = S.length (S.filter p (stream t))
"TEXT length/unstream -> S.length" forall t.
    length (unstream t) = S.length t
"TEXT length/pack -> P.length" forall t.
    length (pack t) = P.length t
"TEXT length/map -> length" forall f t.
    length (map f t) = length t
"TEXT length/zipWith -> length" forall f t1 t2.
    length (zipWith f t1 t2) = min (length t1) (length t2)
"TEXT length/replicate -> n" forall n t.
    length (replicate n t) = mul (max 0 n) (length t)
"TEXT length/cons -> length+1" forall c t.
    length (cons c t) = 1 + length t
"TEXT length/intersperse -> 2*length-1" forall c t.
    length (intersperse c t) = max 0 (mul 2 (length t) - 1)
"TEXT length/intercalate -> n*length" forall s ts.
    length (intercalate s ts) = let lenS = length s in max 0 (P.sum (P.map (\t -> length t + lenS) ts) - lenS)
"TEXT length/empty -> 0"
    length empty = 0
  #-}

-- | /O(min(n,c))/ Compare the count of characters in a 'Text' to a number.
--
-- @
-- 'compareLength' t c = 'P.compare' ('length' t) c
-- @
--
-- This function gives the same answer as comparing against the result
-- of 'length', but can short circuit if the count of characters is
-- greater than the number, and hence be more efficient.
compareLength :: Text -> Int -> Ordering
compareLength t c = S.compareLengthI (stream t) c
{-# INLINE [1] compareLength #-}

{-# RULES
"TEXT compareN/length -> compareLength" [~1] forall t n.
    compare (length t) n = compareLength t n
  #-}

{-# RULES
"TEXT ==N/length -> compareLength/==EQ" [~1] forall t n.
    eqInt (length t) n = compareLength t n == EQ
  #-}

{-# RULES
"TEXT /=N/length -> compareLength//=EQ" [~1] forall t n.
    neInt (length t) n = compareLength t n /= EQ
  #-}

{-# RULES
"TEXT <N/length -> compareLength/==LT" [~1] forall t n.
    ltInt (length t) n = compareLength t n == LT
  #-}

{-# RULES
"TEXT <=N/length -> compareLength//=GT" [~1] forall t n.
    leInt (length t) n = compareLength t n /= GT
  #-}

{-# RULES
"TEXT >N/length -> compareLength/==GT" [~1] forall t n.
    gtInt (length t) n = compareLength t n == GT
  #-}

{-# RULES
"TEXT >=N/length -> compareLength//=LT" [~1] forall t n.
    geInt (length t) n = compareLength t n /= LT
  #-}

-- -----------------------------------------------------------------------------
-- * Transformations
-- | /O(n)/ 'map' @f@ @t@ is the 'Text' obtained by applying @f@ to
-- each element of @t@.
--
-- Example:
--
-- >>> let message = pack "I am not angry. Not at all."
-- >>> T.map (\c -> if c == '.' then '!' else c) message
-- "I am not angry! Not at all!"
--
-- Performs replacement on invalid scalar values.
map :: (Char -> Char) -> Text -> Text
map f = \t -> if null t then empty else mapNonEmpty f t
{-# INLINE [1] map #-}

{-# RULES
"TEXT map/map -> map" forall f g t.
    map f (map g t) = map (f . safe . g) t
#-}

-- | /O(n)/ The 'intercalate' function takes a 'Text' and a list of
-- 'Text's and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- Example:
--
-- >>> T.intercalate "NI!" ["We", "seek", "the", "Holy", "Grail"]
-- "WeNI!seekNI!theNI!HolyNI!Grail"
intercalate :: Text -> [Text] -> Text
intercalate t = concat . L.intersperse t
{-# INLINE [1] intercalate #-}

-- | /O(n)/ The 'intersperse' function takes a character and places it
-- between the characters of a 'Text'.
--
-- Example:
--
-- >>> T.intersperse '.' "SHIELD"
-- "S.H.I.E.L.D"
--
-- Performs replacement on invalid scalar values.
intersperse :: Char -> Text -> Text
intersperse c t@(Text src o l) = if null t then empty else runST $ do
    let !cLen = utf8Length c
        dstLen = l + length t P.* cLen

    dst <- A.new dstLen

    let writeSep = case cLen of
          1 -> \dstOff ->
            A.unsafeWrite dst dstOff (ord8 c)
          2 -> let (c0, c1) = ord2 c in \dstOff -> do
            A.unsafeWrite dst dstOff c0
            A.unsafeWrite dst (dstOff + 1) c1
          3 -> let (c0, c1, c2) = ord3 c in \dstOff -> do
            A.unsafeWrite dst dstOff c0
            A.unsafeWrite dst (dstOff + 1) c1
            A.unsafeWrite dst (dstOff + 2) c2
          _ -> let (c0, c1, c2, c3) = ord4 c in \dstOff -> do
            A.unsafeWrite dst dstOff c0
            A.unsafeWrite dst (dstOff + 1) c1
            A.unsafeWrite dst (dstOff + 2) c2
            A.unsafeWrite dst (dstOff + 3) c3
    let go !srcOff !dstOff = if srcOff >= o + l then return () else do
          let m0 = A.unsafeIndex src srcOff
              m1 = A.unsafeIndex src (srcOff + 1)
              m2 = A.unsafeIndex src (srcOff + 2)
              m3 = A.unsafeIndex src (srcOff + 3)
              !d = utf8LengthByLeader m0
          case d of
            1 -> do
              A.unsafeWrite dst dstOff m0
              writeSep (dstOff + 1)
              go (srcOff + 1) (dstOff + 1 + cLen)
            2 -> do
              A.unsafeWrite dst dstOff m0
              A.unsafeWrite dst (dstOff + 1) m1
              writeSep (dstOff + 2)
              go (srcOff + 2) (dstOff + 2 + cLen)
            3 -> do
              A.unsafeWrite dst dstOff m0
              A.unsafeWrite dst (dstOff + 1) m1
              A.unsafeWrite dst (dstOff + 2) m2
              writeSep (dstOff + 3)
              go (srcOff + 3) (dstOff + 3 + cLen)
            _ -> do
              A.unsafeWrite dst dstOff m0
              A.unsafeWrite dst (dstOff + 1) m1
              A.unsafeWrite dst (dstOff + 2) m2
              A.unsafeWrite dst (dstOff + 3) m3
              writeSep (dstOff + 4)
              go (srcOff + 4) (dstOff + 4 + cLen)

    go o 0
    arr <- A.unsafeFreeze dst
    return (Text arr 0 (dstLen - cLen))
{-# INLINE [1] intersperse #-}

-- | /O(m+n)/ Replace every non-overlapping occurrence of @needle@ in
-- @haystack@ with @replacement@.
--
-- This function behaves as though it was defined as follows:
--
-- @
-- replace needle replacement haystack =
--   'intercalate' replacement ('splitOn' needle haystack)
-- @
--
-- As this suggests, each occurrence is replaced exactly once.  So if
-- @needle@ occurs in @replacement@, that occurrence will /not/ itself
-- be replaced recursively:
--
-- >>> replace "oo" "foo" "oo"
-- "foo"
--
-- In cases where several instances of @needle@ overlap, only the
-- first one will be replaced:
--
-- >>> replace "ofo" "bar" "ofofo"
-- "barfo"
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
replace :: HasCallStack
        => Text
        -- ^ @needle@ to search for.  If this string is empty, an
        -- error will occur.
        -> Text
        -- ^ @replacement@ to replace @needle@ with.
        -> Text
        -- ^ @haystack@ in which to search.
        -> Text
replace needle@(Text _      _      neeLen)
               (Text repArr repOff repLen)
      haystack@(Text hayArr hayOff hayLen)
  | neeLen == 0 = emptyError "replace"
  | len == 0 = empty -- if also haystack is empty, we can't just return 'haystack' as worker/wrapper might duplicate it
  | L.null ixs  = haystack
  | otherwise   = Text (A.run x) 0 len
  where
    ixs = indices needle haystack
    len = hayLen - (neeLen - repLen) `mul` L.length ixs
    x :: ST s (A.MArray s)
    x = do
      marr <- A.new len
      let loop (i:is) o d = do
            let d0 = d + i - o
                d1 = d0 + repLen
            A.copyI (i - o) marr d  hayArr (hayOff+o)
            A.copyI repLen  marr d0 repArr repOff
            loop is (i + neeLen) d1
          loop []     o d = A.copyI (len - d) marr d hayArr (hayOff+o)
      loop ixs 0 0
      return marr

-- ----------------------------------------------------------------------------
-- ** Case conversions (folds)

-- $case
--
-- When case converting 'Text' values, do not use combinators like
-- @map toUpper@ to case convert each character of a string
-- individually, as this gives incorrect results according to the
-- rules of some writing systems.  The whole-string case conversion
-- functions from this module, such as @toUpper@, obey the correct
-- case conversion rules.  As a result, these functions may map one
-- input character to two or three output characters. For examples,
-- see the documentation of each function.
--
-- /Note/: In some languages, case conversion is a locale- and
-- context-dependent operation. The case conversion functions in this
-- module are /not/ locale sensitive. Programs that require locale
-- sensitivity should use appropriate versions of the
-- <http://hackage.haskell.org/package/text-icu-0.6.3.7/docs/Data-Text-ICU.html#g:4 case mapping functions from the text-icu package >.

-- | /O(n)/ Convert a string to folded case.
--
-- This function is mainly useful for performing caseless (also known
-- as case insensitive) string comparisons.
--
-- A string @x@ is a caseless match for a string @y@ if and only if:
--
-- @toCaseFold x == toCaseFold y@
--
-- The result string may be longer than the input string, and may
-- differ from applying 'toLower' to the input string.  For instance,
-- the Armenian small ligature \"&#xfb13;\" (men now, U+FB13) is case
-- folded to the sequence \"&#x574;\" (men, U+0574) followed by
-- \"&#x576;\" (now, U+0576), while the Greek \"&#xb5;\" (micro sign,
-- U+00B5) is case folded to \"&#x3bc;\" (small letter mu, U+03BC)
-- instead of itself.
toCaseFold :: Text -> Text
toCaseFold = \t ->
    if null t then empty
    else toCaseFoldNonEmpty t
{-# INLINE toCaseFold #-}

-- | /O(n)/ Convert a string to lower case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, \"&#x130;\" (Latin capital letter I with dot above,
-- U+0130) maps to the sequence \"i\" (Latin small letter i, U+0069)
-- followed by \" &#x307;\" (combining dot above, U+0307).
toLower :: Text -> Text
toLower = \t ->
  if null t then empty
  else toLowerNonEmpty t
{-# INLINE toLower #-}

-- | /O(n)/ Convert a string to upper case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, the German \"&#xdf;\" (eszett, U+00DF) maps to the
-- two-letter sequence \"SS\".
toUpper :: Text -> Text
toUpper = \t ->
  if null t then empty
  else toUpperNonEmpty t
{-# INLINE toUpper #-}

-- | /O(n)/ Convert a string to title case, using simple case
-- conversion.
--
-- The first letter (as determined by 'Data.Char.isLetter')
-- of the input is converted to title case, as is
-- every subsequent letter that immediately follows a non-letter.
-- Every letter that immediately follows another letter is converted
-- to lower case.
--
-- This function is not idempotent.
-- Consider lower-case letter @ŉ@ (U+0149 LATIN SMALL LETTER N PRECEDED BY APOSTROPHE).
-- Then 'T.toTitle' @"ŉ"@ = @"ʼN"@: the first (and the only) letter of the input
-- is converted to title case, becoming two letters.
-- Now @ʼ@ (U+02BC MODIFIER LETTER APOSTROPHE) is a modifier letter
-- and as such is recognised as a letter by 'Data.Char.isLetter',
-- so 'T.toTitle' @"ʼN"@ = @"'n"@.
--
-- The result string may be longer than the input string. For example,
-- the Latin small ligature &#xfb02; (U+FB02) is converted to the
-- sequence Latin capital letter F (U+0046) followed by Latin small
-- letter l (U+006C).
--
-- /Note/: this function does not take language or culture specific
-- rules into account. For instance, in English, different style
-- guides disagree on whether the book name \"The Hill of the Red
-- Fox\" is correctly title cased&#x2014;but this function will
-- capitalize /every/ word.
--
-- @since 1.0.0.0
toTitle :: Text -> Text
toTitle t = unstream (S.toTitle (stream t))
{-# INLINE toTitle #-}

-- | /O(n)/ Left-justify a string to the given length, using the
-- specified fill character on the right.
-- Performs replacement on invalid scalar values.
--
-- Examples:
--
-- >>> justifyLeft 7 'x' "foo"
-- "fooxxxx"
--
-- >>> justifyLeft 3 'x' "foobar"
-- "foobar"
justifyLeft :: Int -> Char -> Text -> Text
justifyLeft k c t
    | len >= k  = t
    | otherwise = t `append` replicateChar (k-len) c
  where len = length t
{-# INLINE [1] justifyLeft #-}

-- | /O(n)/ Right-justify a string to the given length, using the
-- specified fill character on the left.  Performs replacement on
-- invalid scalar values.
--
-- Examples:
--
-- >>> justifyRight 7 'x' "bar"
-- "xxxxbar"
--
-- >>> justifyRight 3 'x' "foobar"
-- "foobar"
justifyRight :: Int -> Char -> Text -> Text
justifyRight k c t
    | len >= k  = t
    | otherwise = replicateChar (k-len) c `append` t
  where len = length t
{-# INLINE justifyRight #-}

-- | /O(n)/ Center a string to the given length, using the specified
-- fill character on either side.  Performs replacement on invalid
-- scalar values.
--
-- Examples:
--
-- >>> center 8 'x' "HS"
-- "xxxHSxxx"
center :: Int -> Char -> Text -> Text
center k c t
    | len >= k  = t
    | otherwise = replicateChar l c `append` t `append` replicateChar r c
  where len = length t
        d   = k - len
        r   = d `quot` 2
        l   = d - r
{-# INLINE center #-}

-- | /O(n)/ The 'transpose' function transposes the rows and columns
-- of its 'Text' argument.  Note that this function uses 'pack',
-- 'unpack', and the list version of transpose, and is thus not very
-- efficient.
--
-- Examples:
--
-- >>> transpose ["green","orange"]
-- ["go","rr","ea","en","ng","e"]
--
-- >>> transpose ["blue","red"]
-- ["br","le","ud","e"]
transpose :: [Text] -> [Text]
transpose ts = P.map pack (L.transpose (P.map unpack ts))

-- -----------------------------------------------------------------------------
-- * Reducing 'Text's (folds)

-- | /O(n)/ 'foldl', applied to a binary operator, a starting value
-- (typically the left-identity of the operator), and a 'Text',
-- reduces the 'Text' using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z t = S.foldl f z (stream t)
{-# INLINE foldl #-}

-- | /O(n)/ A strict version of 'foldl'.
foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z t = S.foldl' f z (stream t)
{-# INLINE foldl' #-}

-- | /O(n)/ A variant of 'foldl' that has no starting value argument,
-- and thus must be applied to a non-empty 'Text'.
foldl1 :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldl1 f t = S.foldl1 f (stream t)
{-# INLINE foldl1 #-}

-- | /O(n)/ A strict version of 'foldl1'.
foldl1' :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldl1' f t = S.foldl1' f (stream t)
{-# INLINE foldl1' #-}

-- | /O(n)/ 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a 'Text',
-- reduces the 'Text' using the binary operator, from right to left.
--
-- If the binary operator is strict in its second argument, use 'foldr''
-- instead.
--
-- 'foldr' is lazy like 'Data.List.foldr' for lists: evaluation actually
-- traverses the 'Text' from left to right, only as far as it needs to.
--
-- For example, 'head' can be defined with /O(1)/ complexity using 'foldr':
--
-- @
-- head :: Text -> Char
-- head = foldr const (error "head empty")
-- @
--
-- Searches from left to right with short-circuiting behavior can
-- also be defined using 'foldr' (/e.g./, 'any', 'all', 'find', 'elem').
foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z t = S.foldr f z (stream t)
{-# INLINE foldr #-}

-- | /O(n)/ A variant of 'foldr' that has no starting value argument,
-- and thus must be applied to a non-empty 'Text'.
foldr1 :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldr1 f t = S.foldr1 f (stream t)
{-# INLINE foldr1 #-}

-- | /O(n)/ A strict version of 'foldr'.
--
-- 'foldr'' evaluates as a right-to-left traversal using constant stack space.
--
-- @since 2.0.1
foldr' :: (Char -> a -> a) -> a -> Text -> a
foldr' f z t = S.foldl' (P.flip f) z (reverseStream t)
{-# INLINE foldr' #-}

-- -----------------------------------------------------------------------------
-- ** Special folds

-- | /O(n)/ Concatenate a list of 'Text's.
concat :: [Text] -> Text
concat ts = case ts of
    [] -> empty
    [t] -> t
    _ | len == 0 -> empty
      | otherwise -> Text (A.run go) 0 len
  where
    len = sumP "concat" $ L.map lengthWord8 ts
    go :: ST s (A.MArray s)
    go = do
      arr <- A.new len
      let step i (Text a o l) = A.copyI l arr i a o >> return (i + l)
      foldM step 0 ts >> return arr

-- | /O(n)/ Map a function over a 'Text' that results in a 'Text', and
-- concatenate the results.
concatMap :: (Char -> Text) -> Text -> Text
concatMap f = concat . foldr ((:) . f) []
{-# INLINE concatMap #-}

-- | /O(n)/ 'any' @p@ @t@ determines whether any character in the
-- 'Text' @t@ satisfies the predicate @p@.
any :: (Char -> Bool) -> Text -> Bool
any p t = S.any p (stream t)
{-# INLINE any #-}

-- | /O(n)/ 'all' @p@ @t@ determines whether all characters in the
-- 'Text' @t@ satisfy the predicate @p@.
all :: (Char -> Bool) -> Text -> Bool
all p t = S.all p (stream t)
{-# INLINE all #-}

-- | /O(n)/ 'maximum' returns the maximum value from a 'Text', which
-- must be non-empty.
maximum :: HasCallStack => Text -> Char
maximum t = S.maximum (stream t)
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'Text', which
-- must be non-empty.
minimum :: HasCallStack => Text -> Char
minimum t = S.minimum (stream t)
{-# INLINE minimum #-}

-- -----------------------------------------------------------------------------
-- * Building 'Text's
-- | /O(n)/ 'scanl' is similar to 'foldl', but returns a list of
-- successive reduced values from the left.
-- Performs replacement on invalid scalar values.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- __Properties__
--
-- @'head' ('scanl' f z xs) = z@
--
-- @'last' ('scanl' f z xs) = 'foldl' f z xs@
scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
scanl f z t = unstream (S.scanl g z (stream t))
    where g a b = safe (f a b)
{-# INLINE scanl #-}

-- | /O(n)/ 'scanl1' is a variant of 'scanl' that has no starting
-- value argument. Performs replacement on invalid scalar values.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Char -> Char -> Char) -> Text -> Text
scanl1 f t | null t    = empty
           | otherwise = scanl f (unsafeHead t) (unsafeTail t)
{-# INLINE scanl1 #-}

-- | /O(n)/ 'scanr' is the right-to-left dual of 'scanl'.  Performs
-- replacement on invalid scalar values.
--
-- > scanr f v == reverse . scanl (flip f) v . reverse
scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
scanr f z = S.reverse . S.reverseScanr g z . reverseStream
    where g a b = safe (f a b)
{-# INLINE scanr #-}

-- | /O(n)/ 'scanr1' is a variant of 'scanr' that has no starting
-- value argument. Performs replacement on invalid scalar values.
scanr1 :: (Char -> Char -> Char) -> Text -> Text
scanr1 f t | null t    = empty
           | otherwise = scanr f (last t) (init t)
{-# INLINE scanr1 #-}

-- | /O(n)/ Like a combination of 'map' and 'foldl''. Applies a
-- function to each element of a 'Text', passing an accumulating
-- parameter from left to right, and returns a final 'Text'.  Performs
-- replacement on invalid scalar values.
mapAccumL :: forall a. (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
mapAccumL f z0 = go
  where
    go (Text src o l) = runST $ do
      marr <- A.new (l + 4)
      outer marr (l + 4) o 0 z0
      where
        outer :: forall s. A.MArray s -> Int -> Int -> Int -> a -> ST s (a, Text)
        outer !dst !dstLen = inner
          where
            inner !srcOff !dstOff !z
              | srcOff >= l + o = do
                A.shrinkM dst dstOff
                arr <- A.unsafeFreeze dst
                return (z, Text arr 0 dstOff)
              | dstOff + 4 > dstLen = do
                let !dstLen' = dstLen + (l + o) - srcOff + 4
                dst' <- A.resizeM dst dstLen'
                outer dst' dstLen' srcOff dstOff z
              | otherwise = do
                let !(Iter c d) = iterArray src srcOff
                    (z', c') = f z c
                d' <- unsafeWrite dst dstOff (safe c')
                inner (srcOff + d) (dstOff + d') z'
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- a strict 'foldr'; it applies a function to each element of a
-- 'Text', passing an accumulating parameter from right to left, and
-- returning a final value of this accumulator together with the new
-- 'Text'.
-- Performs replacement on invalid scalar values.
mapAccumR :: forall a. (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
mapAccumR f z0 = go
  where
    go (Text src o l) = runST $ do
      marr <- A.new (l + 4)
      outer marr (l + o - 1) (l + 4 - 1) z0
      where
        outer :: forall s. A.MArray s -> Int -> Int -> a -> ST s (a, Text)
        outer !dst = inner
          where
            inner !srcOff !dstOff !z
              | srcOff < o = do
                dstLen <- A.getSizeofMArray dst
                arr <- A.unsafeFreeze dst
                return (z, Text arr (dstOff + 1) (dstLen - dstOff - 1))
              | dstOff < 3 = do
                dstLen <- A.getSizeofMArray dst
                let !dstLen' = dstLen + (srcOff - o) + 4
                dst' <- A.new dstLen'
                A.copyM dst' (dstLen' - dstLen) dst 0 dstLen
                outer dst' srcOff (dstOff + dstLen' - dstLen) z
              | otherwise = do
                let !(Iter c d) = reverseIterArray src (srcOff)
                    (z', c') = f z c
                    c'' = safe c'
                    !d' = utf8Length c''
                    dstOff' = dstOff - d'
                _ <- unsafeWrite dst (dstOff' + 1) c''
                inner (srcOff + d) dstOff' z'
{-# INLINE mapAccumR #-}

-- -----------------------------------------------------------------------------
-- ** Generating and unfolding 'Text's

-- | /O(n*m)/ 'replicate' @n@ @t@ is a 'Text' consisting of the input
-- @t@ repeated @n@ times.
replicate :: Int -> Text -> Text
replicate n t@(Text a o l)
    | n <= 0 || l <= 0       = empty
    | n == 1                 = t
    | isSingleton t          = replicateChar n (unsafeHead t)
    | otherwise              = runST $ do
        let totalLen = n `mul` l
        marr <- A.new totalLen
        A.copyI l marr 0 a o
        A.tile marr l
        arr  <- A.unsafeFreeze marr
        return $ Text arr 0 totalLen
{-# INLINE [1] replicate #-}

{-# RULES
"TEXT replicate/singleton -> replicateChar" [~1] forall n c.
    replicate n (singleton c) = replicateChar n c
  #-}

-- | /O(n)/ 'replicateChar' @n@ @c@ is a 'Text' of length @n@ with @c@ the
-- value of every element.
replicateChar :: Int -> Char -> Text
replicateChar !len !c'
  | len <= 0  = empty
  | Char.isAscii c = runST $ do
    marr <- A.newFilled len (Char.ord c)
    arr  <- A.unsafeFreeze marr
    return $ Text arr 0 len
  | otherwise = runST $ do
    let cLen = utf8Length c
        totalLen = cLen P.* len
    marr <- A.new totalLen
    _ <- unsafeWrite marr 0 c
    A.tile marr cLen
    arr  <- A.unsafeFreeze marr
    return $ Text arr 0 totalLen
  where
    c = safe c'
{-# INLINE replicateChar #-}

-- | /O(n)/, where @n@ is the length of the result. The 'unfoldr'
-- function is analogous to the List 'L.unfoldr'. 'unfoldr' builds a
-- 'Text' from a seed value. The function takes the element and
-- returns 'Nothing' if it is done producing the 'Text', otherwise
-- 'Just' @(a,b)@.  In this case, @a@ is the next 'Char' in the
-- string, and @b@ is the seed value for further production.
-- Performs replacement on invalid scalar values.
unfoldr     :: (a -> Maybe (Char,a)) -> a -> Text
unfoldr f s = unstream (S.unfoldr (firstf safe . f) s)
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a 'Text' from a seed
-- value. However, the length of the result should be limited by the
-- first argument to 'unfoldrN'. This function is more efficient than
-- 'unfoldr' when the maximum length of the result is known and
-- correct, otherwise its performance is similar to 'unfoldr'.
-- Performs replacement on invalid scalar values.
unfoldrN     :: Int -> (a -> Maybe (Char,a)) -> a -> Text
unfoldrN n f s = unstream (S.unfoldrN n (firstf safe . f) s)
{-# INLINE unfoldrN #-}

-- -----------------------------------------------------------------------------
-- * Substrings

-- | /O(n)/ 'take' @n@, applied to a 'Text', returns the prefix of the
-- 'Text' of length @n@, or the 'Text' itself if @n@ is greater than
-- the length of the Text.
take :: Int -> Text -> Text
take n t@(Text arr off len)
    | n <= 0    = empty
    | n >= len || m >= len || m < 0  = t
    | otherwise = Text arr off m 
  where
    m = measureOff n t
{-# INLINE [1] take #-}

-- | /O(n)/ If @t@ is long enough to contain @n@ characters, 'measureOff' @n@ @t@
-- returns a non-negative number, measuring their size in 'Word8'. Otherwise,
-- if @t@ is shorter, return a non-positive number, which is a negated total count
-- of 'Char' available in @t@. If @t@ is empty or @n = 0@, return 0.
--
-- This function is used to implement 'take', 'drop', 'splitAt' and 'length'
-- and is useful on its own in streaming and parsing libraries.
--
-- @since 2.0
measureOff :: Int -> Text -> Int
measureOff !n (Text (A.ByteArray arr) off len) = if len == 0 then 0 else
  cSsizeToInt $
    measure_off arr (intToCSize off) (intToCSize len) (intToCSize n)

-- | /O(n)/ 'takeEnd' @n@ @t@ returns the suffix remaining after
-- taking @n@ characters from the end of @t@.
--
-- Examples:
--
-- >>> takeEnd 3 "foobar"
-- "bar"
--
-- @since 1.1.1.0
takeEnd :: Int -> Text -> Text
takeEnd n t@(Text arr off len)
    | n <= 0    = empty
    | n >= len  = t
    | otherwise = text arr (off+i) (len-i)
  where i = iterNEnd n t

iterNEnd :: Int -> Text -> Int
iterNEnd n t@(Text _arr _off len) = loop (len-1) n
  where loop i !m
          | m <= 0    = i+1
          | i <= 0    = 0
          | otherwise = loop (i+d) (m-1)
          where d = reverseIter_ t i

-- | /O(n)/ 'drop' @n@, applied to a 'Text', returns the suffix of the
-- 'Text' after the first @n@ characters, or the empty 'Text' if @n@
-- is greater than the length of the 'Text'.
drop :: Int -> Text -> Text
drop n t@(Text arr off len)
    | n <= 0    = t
    | n >= len || m >= len || m < 0 = empty
    | otherwise = Text arr (off+m) (len-m) 
  where m = measureOff n t
{-# INLINE [1] drop #-}

-- | /O(n)/ 'dropEnd' @n@ @t@ returns the prefix remaining after
-- dropping @n@ characters from the end of @t@.
--
-- Examples:
--
-- >>> dropEnd 3 "foobar"
-- "foo"
--
-- @since 1.1.1.0
dropEnd :: Int -> Text -> Text
dropEnd n t@(Text arr off len)
    | n <= 0    = t
    | n >= len  = empty
    | otherwise = text arr off (iterNEnd n t)

-- | /O(n)/ 'takeWhile', applied to a predicate @p@ and a 'Text',
-- returns the longest prefix (possibly empty) of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p t@(Text arr off len) = loop 0
  where loop !i | i >= len    = t
                | p c         = loop (i+d)
                | otherwise   = text arr off i
            where Iter c d    = iter t i
{-# INLINE [1] takeWhile #-}

-- | /O(n)/ 'takeWhileEnd', applied to a predicate @p@ and a 'Text',
-- returns the longest suffix (possibly empty) of elements that
-- satisfy @p@.
-- Examples:
--
-- >>> takeWhileEnd (=='o') "foo"
-- "oo"
--
-- @since 1.2.2.0
takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p t@(Text arr off len) = loop (len-1) len
  where loop !i !l | l <= 0    = t
                   | p c       = loop (i+d) (l+d)
                   | otherwise = text arr (off+l) (len-l)
            where Iter c d     = reverseIter t i
{-# INLINE [1] takeWhileEnd #-}

-- | /O(n)/ 'dropWhile' @p@ @t@ returns the suffix remaining after
-- 'takeWhile' @p@ @t@.
dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p t@(Text arr off len) = loop 0 0
  where loop !i !l | l >= len  = empty
                   | p c       = loop (i+d) (l+d)
                   | otherwise = Text arr (off+i) (len-l)
            where Iter c d     = iter t i
{-# INLINE [1] dropWhile #-}

-- | /O(n)/ 'dropWhileEnd' @p@ @t@ returns the prefix remaining after
-- dropping characters that satisfy the predicate @p@ from the end of
-- @t@.
--
-- Examples:
--
-- >>> dropWhileEnd (=='.') "foo..."
-- "foo"
dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p t@(Text arr off len) = loop (len-1) len
  where loop !i !l | l <= 0    = empty
                   | p c       = loop (i+d) (l+d)
                   | otherwise = Text arr off l
            where Iter c d     = reverseIter t i
{-# INLINE [1] dropWhileEnd #-}

-- | /O(n)/ 'dropAround' @p@ @t@ returns the substring remaining after
-- dropping characters that satisfy the predicate @p@ from both the
-- beginning and end of @t@.
dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p
{-# INLINE [1] dropAround #-}

-- | /O(n)/ Remove leading white space from a string.  Equivalent to:
--
-- > dropWhile isSpace
stripStart :: Text -> Text
stripStart = dropWhile Char.isSpace
{-# INLINE stripStart #-}

-- | /O(n)/ Remove trailing white space from a string.  Equivalent to:
--
-- > dropWhileEnd isSpace
stripEnd :: Text -> Text
stripEnd = dropWhileEnd Char.isSpace
{-# INLINE [1] stripEnd #-}

-- | /O(n)/ Remove leading and trailing white space from a string.
-- Equivalent to:
--
-- > dropAround isSpace
strip :: Text -> Text
strip = dropAround Char.isSpace
{-# INLINE [1] strip #-}

-- | /O(n)/ 'splitAt' @n t@ returns a pair whose first element is a
-- prefix of @t@ of length @n@, and whose second is the remainder of
-- the string. It is equivalent to @('take' n t, 'drop' n t)@.
splitAt :: Int -> Text -> (Text, Text)
splitAt n t@(Text arr off len)
    | n <= 0    = (empty, t)
    | n >= len || m >= len || m < 0  = (t, empty)
    | otherwise = (Text arr off m, Text arr (off+m) (len-m)) 
  where
    m = measureOff n t

-- | /O(n)/ 'span', applied to a predicate @p@ and text @t@, returns
-- a pair whose first element is the longest prefix (possibly empty)
-- of @t@ of elements that satisfy @p@, and whose second is the
-- remainder of the text.
--
-- >>> T.span (=='0') "000AB"
-- ("000","AB")
span :: (Char -> Bool) -> Text -> (Text, Text)
span p t = case span_ p t of
             (# hd,tl #) -> (hd,tl)
{-# INLINE span #-}

-- | /O(n)/ 'break' is like 'span', but the prefix returned is
-- over elements that fail the predicate @p@.
--
-- >>> T.break (=='c') "180cm"
-- ("180","cm")
break :: (Char -> Bool) -> Text -> (Text, Text)
break p = span (not . p)
{-# INLINE break #-}

-- | /O(length of prefix)/ 'spanM', applied to a monadic predicate @p@,
-- a text @t@, returns a pair @(t1, t2)@ where @t1@ is the longest prefix of
-- @t@ whose elements satisfy @p@, and @t2@ is the remainder of the text.
--
-- >>> T.spanM (\c -> state $ \i -> (fromEnum c == i, i+1)) "abcefg" `runState` 97
-- (("abc","efg"),101)
--
-- 'span' is 'spanM' specialized to 'Data.Functor.Identity.Identity':
--
-- @
-- -- for all p :: Char -> Bool
-- 'span' p = 'Data.Functor.Identity.runIdentity' . 'spanM' ('pure' . p)
-- @
--
-- @since 2.0.1
spanM :: Monad m => (Char -> m Bool) -> Text -> m (Text, Text)
spanM p t@(Text arr off len) = go 0
  where
    go !i | i < len = case iterArray arr (off+i) of
        Iter c l -> do
            continue <- p c
            if continue then go (i+l)
            else pure (text arr off i, text arr (off+i) (len-i))
    go _ = pure (t, empty)
{-# INLINE spanM #-}

-- | /O(length of suffix)/ 'spanEndM', applied to a monadic predicate @p@,
-- a text @t@, returns a pair @(t1, t2)@ where @t2@ is the longest suffix of
-- @t@ whose elements satisfy @p@, and @t1@ is the remainder of the text.
--
-- >>> T.spanEndM (\c -> state $ \i -> (fromEnum c == i, i-1)) "tuvxyz" `runState` 122
-- (("tuv","xyz"),118)
--
-- @
-- 'spanEndM' p . 'reverse' = fmap ('Data.Bifunctor.bimap' 'reverse' 'reverse') . 'spanM' p
-- @
--
-- @since 2.0.1
spanEndM :: Monad m => (Char -> m Bool) -> Text -> m (Text, Text)
spanEndM p t@(Text arr off len) = go (len-1)
  where
    go !i | 0 <= i = case reverseIterArray arr (off+i) of
        Iter c l -> do
            continue <- p c
            if continue then go (i+l)
            else pure (text arr off (i+1), text arr (off+i+1) (len-i-1))
    go _ = pure (empty, t)
{-# INLINE spanEndM #-}

-- | /O(n)/ Group characters in a string according to a predicate.
groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy p = loop
  where
    loop t@(Text arr off len)
        | null t    = []
        | otherwise = text arr off n : loop (text arr (off+n) (len-n))
        where Iter c d = iter t 0
              n     = d + findAIndexOrEnd (not . p c) (Text arr (off+d) (len-d))

-- | Returns the /array/ index (in units of 'Word8') at which a
-- character may be found.  This is /not/ the same as the logical
-- index returned by e.g. 'findIndex'.
findAIndexOrEnd :: (Char -> Bool) -> Text -> Int
findAIndexOrEnd q t@(Text _arr _off len) = go 0
    where go !i | i >= len || q c       = i
                | otherwise             = go (i+d)
                where Iter c d          = iter t i

-- | /O(n)/ Group characters in a string by equality.
group :: Text -> [Text]
group = groupBy (==)

-- | /O(n)/ Return all initial segments of the given 'Text', shortest
-- first.
inits :: Text -> [Text]
inits t = empty : case t of
  Text arr off len ->
    let loop i | i >= len = []
               | otherwise = let !j = i + iter_ t i in Text arr off j : loop j
    in loop 0

-- | /O(n)/ Return all final segments of the given 'Text', longest
-- first.
tails :: Text -> [Text]
tails t | null t    = [empty]
        | otherwise = t : tails (unsafeTail t)

-- $split
--
-- Splitting functions in this library do not perform character-wise
-- copies to create substrings; they just construct new 'Text's that
-- are slices of the original.

-- | /O(m+n)/ Break a 'Text' into pieces separated by the first 'Text'
-- argument (which cannot be empty), consuming the delimiter. An empty
-- delimiter is invalid, and will cause an error to be raised.
--
-- Examples:
--
-- >>> splitOn "\r\n" "a\r\nb\r\nd\r\ne"
-- ["a","b","d","e"]
--
-- >>> splitOn "aaa"  "aaaXaaaXaaaXaaa"
-- ["","X","X","X",""]
--
-- >>> splitOn "x"    "x"
-- ["",""]
--
-- and
--
-- > intercalate s . splitOn s         == id
-- > splitOn (singleton c)             == split (==c)
--
-- (Note: the string @s@ to split on above cannot be empty.)
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
splitOn :: HasCallStack
        => Text
        -- ^ String to split on. If this string is empty, an error
        -- will occur.
        -> Text
        -- ^ Input text.
        -> [Text]
splitOn pat@(Text _ _ l) src@(Text arr off len)
    | l <= 0          = emptyError "splitOn"
    | isSingleton pat = split (== unsafeHead pat) src
    | otherwise       = go 0 (indices pat src)
  where
    go !s (x:xs) =  text arr (s+off) (x-s) : go (x+l) xs
    go  s _      = [text arr (s+off) (len-s)]
{-# INLINE [1] splitOn #-}

{-# RULES
"TEXT splitOn/singleton -> split/==" [~1] forall c t.
    splitOn (singleton c) t = split (==c) t
  #-}

-- | /O(n)/ Splits a 'Text' into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- >>> split (=='a') "aabbaca"
-- ["","","bb","c",""]
--
-- >>> split (=='a') ""
-- [""]
split :: (Char -> Bool) -> Text -> [Text]
split p t
    | null t = [empty]
    | otherwise = loop t
    where loop s | null s'   = [l]
                 | otherwise = l : loop (unsafeTail s')
              where (# l, s' #) = span_ (not . p) s
{-# INLINE split #-}

-- | /O(n)/ Splits a 'Text' into components of length @k@.  The last
-- element may be shorter than the other chunks, depending on the
-- length of the input. Examples:
--
-- >>> chunksOf 3 "foobarbaz"
-- ["foo","bar","baz"]
--
-- >>> chunksOf 4 "haskell.org"
-- ["hask","ell.","org"]
chunksOf :: Int -> Text -> [Text]
chunksOf k = go
  where
    go t = case splitAt k t of
             (a,b) | null a    -> []
                   | otherwise -> a : go b
{-# INLINE chunksOf #-}

-- ----------------------------------------------------------------------------
-- * Searching

-------------------------------------------------------------------------------
-- ** Searching with a predicate

-- | /O(n)/ The 'elem' function takes a character and a 'Text', and
-- returns 'True' if the element is found in the given 'Text', or
-- 'False' otherwise.
elem :: Char -> Text -> Bool
elem c t = S.any (== c) (stream t)
{-# INLINE elem #-}

-- | /O(n)/ The 'find' function takes a predicate and a 'Text', and
-- returns the first element matching the predicate, or 'Nothing' if
-- there is no such element.
find :: (Char -> Bool) -> Text -> Maybe Char
find p t = S.findBy p (stream t)
{-# INLINE find #-}

-- | /O(n)/ The 'partition' function takes a predicate and a 'Text',
-- and returns the pair of 'Text's with elements which do and do not
-- satisfy the predicate, respectively; i.e.
--
-- > partition p t == (filter p t, filter (not . p) t)
partition :: (Char -> Bool) -> Text -> (Text, Text)
partition p t = (filter p t, filter (not . p) t)
{-# INLINE partition #-}

-- | /O(n)/ 'filter', applied to a predicate and a 'Text',
-- returns a 'Text' containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> Text -> Text
filter p = filter_ text p
{-# INLINE [1] filter #-}

{-# RULES
"TEXT filter/filter -> filter" forall p q t.
    filter p (filter q t) = filter (\c -> q c && p c) t
#-}

-- | /O(n+m)/ Find the first instance of @needle@ (which must be
-- non-'null') in @haystack@.  The first element of the returned tuple
-- is the prefix of @haystack@ before @needle@ is matched.  The second
-- is the remainder of @haystack@, starting with the match.
--
-- Examples:
--
-- >>> breakOn "::" "a::b::c"
-- ("a","::b::c")
--
-- >>> breakOn "/" "foobar"
-- ("foobar","")
--
-- Laws:
--
-- > append prefix match == haystack
-- >   where (prefix, match) = breakOn needle haystack
--
-- If you need to break a string by a substring repeatedly (e.g. you
-- want to break on every instance of a substring), use 'breakOnAll'
-- instead, as it has lower startup overhead.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
breakOn :: HasCallStack => Text -> Text -> (Text, Text)
breakOn pat src@(Text arr off len)
    | null pat  = emptyError "breakOn"
    | otherwise = case indices pat src of
                    []    -> (src, empty)
                    (x:_) -> (text arr off x, text arr (off+x) (len-x))
{-# INLINE breakOn #-}

-- | /O(n+m)/ Similar to 'breakOn', but searches from the end of the
-- string.
--
-- The first element of the returned tuple is the prefix of @haystack@
-- up to and including the last match of @needle@.  The second is the
-- remainder of @haystack@, following the match.
--
-- >>> breakOnEnd "::" "a::b::c"
-- ("a::b::","c")
breakOnEnd :: HasCallStack => Text -> Text -> (Text, Text)
breakOnEnd pat src = (reverse b, reverse a)
    where (a,b) = breakOn (reverse pat) (reverse src)
{-# INLINE breakOnEnd #-}

-- | /O(n+m)/ Find all non-overlapping instances of @needle@ in
-- @haystack@.  Each element of the returned list consists of a pair:
--
-- * The entire string prior to the /k/th match (i.e. the prefix)
--
-- * The /k/th match, followed by the remainder of the string
--
-- Examples:
--
-- >>> breakOnAll "::" ""
-- []
--
-- >>> breakOnAll "/" "a/b/c/"
-- [("a","/b/c/"),("a/b","/c/"),("a/b/c","/")]
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
--
-- The @needle@ parameter may not be empty.
breakOnAll :: HasCallStack
           => Text              -- ^ @needle@ to search for
           -> Text              -- ^ @haystack@ in which to search
           -> [(Text, Text)]
breakOnAll pat src@(Text arr off slen)
    | null pat  = emptyError "breakOnAll"
    | otherwise = L.map step (indices pat src)
  where
    step       x = (chunk 0 x, chunk x (slen-x))
    chunk !n !l  = text arr (n+off) l
{-# INLINE breakOnAll #-}

-------------------------------------------------------------------------------
-- ** Indexing 'Text's

-- $index
--
-- If you think of a 'Text' value as an array of 'Char' values (which
-- it is not), you run the risk of writing inefficient code.
--
-- An idiom that is common in some languages is to find the numeric
-- offset of a character or substring, then use that number to split
-- or trim the searched string.  With a 'Text' value, this approach
-- would require two /O(n)/ operations: one to perform the search, and
-- one to operate from wherever the search ended.
--
-- For example, suppose you have a string that you want to split on
-- the substring @\"::\"@, such as @\"foo::bar::quux\"@. Instead of
-- searching for the index of @\"::\"@ and taking the substrings
-- before and after that index, you would instead use @breakOnAll \"::\"@.

-- | /O(n)/ 'Text' index (subscript) operator, starting from 0.
index :: HasCallStack => Text -> Int -> Char
index t n = S.index (stream t) n
{-# INLINE index #-}

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'Text'
-- and returns the index of the first element in the 'Text' satisfying
-- the predicate.
findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex p t = S.findIndex p (stream t)
{-# INLINE findIndex #-}

-- | /O(n+m)/ The 'count' function returns the number of times the
-- query string appears in the given 'Text'. An empty query string is
-- invalid, and will cause an error to be raised.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
count :: HasCallStack => Text -> Text -> Int
count pat
    | null pat        = emptyError "count"
    | isSingleton pat = countChar (unsafeHead pat)
    | otherwise       = L.length . indices pat
{-# INLINE [1] count #-}

{-# RULES
"TEXT count/singleton -> countChar" [~1] forall c t.
    count (singleton c) t = countChar c t
  #-}

-- | /O(n)/ The 'countChar' function returns the number of times the
-- query element appears in the given 'Text'.
countChar :: Char -> Text -> Int
countChar c t = S.countChar c (stream t)
{-# INLINE countChar #-}

-------------------------------------------------------------------------------
-- * Zipping

-- | /O(n)/ 'zip' takes two 'Text's and returns a list of
-- corresponding pairs of bytes. If one input 'Text' is short,
-- excess elements of the longer 'Text' are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: Text -> Text -> [(Char,Char)]
zip a b = S.unstreamList $ S.zipWith (,) (stream a) (stream b)
{-# INLINE zip #-}

-- | /O(n)/ 'zipWith' generalises 'zip' by zipping with the function
-- given as the first argument, instead of a tupling function.
-- Performs replacement on invalid scalar values.
zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f t1 t2 = unstream (S.zipWith g (stream t1) (stream t2))
    where g a b = safe (f a b)
{-# INLINE [1] zipWith #-}

-- | /O(n)/ Breaks a 'Text' up into a list of words, delimited by 'Char's
-- representing white space.
words :: Text -> [Text]
words (Text arr off len) = loop 0 0
  where
    loop !start !n
        | n >= len = if start == n
                     then []
                     else [Text arr (start + off) (n - start)]
        -- Spaces in UTF-8 take either 1 byte for 0x09..0x0D + 0x20
        | isAsciiSpace w0 =
            if start == n
            then loop (n + 1) (n + 1)
            else Text arr (start + off) (n - start) : loop (n + 1) (n + 1)
        | w0 < 0x80 = loop start (n + 1)
        -- or 2 bytes for 0xA0
        | w0 == 0xC2, w1 == 0xA0 =
            if start == n
            then loop (n + 2) (n + 2)
            else Text arr (start + off) (n - start) : loop (n + 2) (n + 2)
        | w0 < 0xE0 = loop start (n + 2)
        -- or 3 bytes for 0x1680 + 0x2000..0x200A + 0x2028..0x2029 + 0x202F + 0x205F + 0x3000
        |  w0 == 0xE1 && w1 == 0x9A && w2 == 0x80
        || w0 == 0xE2 && (w1 == 0x80 && Char.isSpace (chr3 w0 w1 w2) || w1 == 0x81 && w2 == 0x9F)
        || w0 == 0xE3 && w1 == 0x80 && w2 == 0x80 =
            if start == n
            then loop (n + 3) (n + 3)
            else Text arr (start + off) (n - start) : loop (n + 3) (n + 3)
        | otherwise = loop start (n + utf8LengthByLeader w0)
        where
            w0 = A.unsafeIndex arr (off + n)
            w1 = A.unsafeIndex arr (off + n + 1)
            w2 = A.unsafeIndex arr (off + n + 2)
{-# INLINE words #-}

-- Adapted from Data.ByteString.Internal.isSpaceWord8
isAsciiSpace :: Word8 -> Bool
isAsciiSpace w = w .&. 0x50 == 0 && w < 0x80 && (w == 0x20 || w - 0x09 < 5)
{-# INLINE isAsciiSpace #-}

-- | /O(n)/ Breaks a 'Text' up into a list of 'Text's at newline characters
-- @'\\n'@ (LF, line feed). The resulting strings do not contain newlines.
--
-- 'lines' __does not__ treat @'\\r'@ (CR, carriage return) as a newline character.
lines :: Text -> [Text]
lines (Text arr@(A.ByteArray arr#) off len) = go off
  where
    go !n
      | n >= len + off = []
      | delta < 0 = [Text arr n (len + off - n)]
      | otherwise = Text arr n delta : go (n + delta + 1)
      where
        delta = memchr arr# n (len + off - n) 0x0A
{-# INLINE lines #-}

-- | /O(n)/ Joins lines, after appending a terminating newline to
-- each.
unlines :: [Text] -> Text
unlines = concat . L.foldr (\t acc -> t : singleton '\n' : acc) []
{-# INLINE unlines #-}

-- | /O(n)/ Joins words using single space characters.
unwords :: [Text] -> Text
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

-- | /O(n)/ The 'isPrefixOf' function takes two 'Text's and returns
-- 'True' if and only if the first is a prefix of the second.
isPrefixOf :: Text -> Text -> Bool
isPrefixOf a@(Text _ _ alen) b@(Text _ _ blen) =
    alen <= blen && S.isPrefixOf (stream a) (stream b)
{-# INLINE [1] isPrefixOf #-}

-- | /O(n)/ The 'isSuffixOf' function takes two 'Text's and returns
-- 'True' if and only if the first is a suffix of the second.
isSuffixOf :: Text -> Text -> Bool
isSuffixOf a@(Text _aarr _aoff alen) b@(Text barr boff blen) =
    d >= 0 && a == b'
  where d              = blen - alen
        b' | d == 0    = b
           | otherwise = Text barr (boff+d) alen
{-# INLINE isSuffixOf #-}

-- | /O(n+m)/ The 'isInfixOf' function takes two 'Text's and returns
-- 'True' if and only if the first is contained, wholly and intact, anywhere
-- within the second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
isInfixOf ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> Text -> Bool
isInfixOf needle haystack
    | null needle        = True
    | isSingleton needle = S.elem (unsafeHead needle) . S.stream $ haystack
    | otherwise          = not . L.null . indices needle $ haystack
{-# INLINE [1] isInfixOf #-}

-------------------------------------------------------------------------------
-- * View patterns

-- | /O(n)/ Return the suffix of the second string if its prefix
-- matches the entire first string.
--
-- Examples:
--
-- >>> stripPrefix "foo" "foobar"
-- Just "bar"
--
-- >>> stripPrefix ""    "baz"
-- Just "baz"
--
-- >>> stripPrefix "foo" "quux"
-- Nothing
--
-- This is particularly useful with the @ViewPatterns@ extension to
-- GHC, as follows:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > import Data.Text as T
-- >
-- > fnordLength :: Text -> Int
-- > fnordLength (stripPrefix "fnord" -> Just suf) = T.length suf
-- > fnordLength _                                 = -1
stripPrefix :: Text -> Text -> Maybe Text
stripPrefix p@(Text _arr _off plen) t@(Text arr off len)
    | p `isPrefixOf` t = Just $! text arr (off+plen) (len-plen)
    | otherwise        = Nothing

-- | /O(n)/ Find the longest non-empty common prefix of two strings
-- and return it, along with the suffixes of each string at which they
-- no longer match.
--
-- If the strings do not have a common prefix or either one is empty,
-- this function returns 'Nothing'.
--
-- Examples:
--
-- >>> commonPrefixes "foobar" "fooquux"
-- Just ("foo","bar","quux")
--
-- >>> commonPrefixes "veeble" "fetzer"
-- Nothing
--
-- >>> commonPrefixes "" "baz"
-- Nothing
commonPrefixes :: Text -> Text -> Maybe (Text, Text, Text)
commonPrefixes !t0@(Text arr0 off0 len0) !t1@(Text arr1 off1 len1)
  | len0 == 0 = Nothing
  | len1 == 0 = Nothing
  | otherwise = go 0 0
  where
    go !i !j
      | i == len0 = Just (t0, empty, text arr1 (off1 + i) (len1 - i))
      | i == len1 = Just (t1, text arr0 (off0 + i) (len0 - i), empty)
      | a == b = go (i + 1) k
      | k > 0 = Just (Text arr0 off0 k,
                      Text arr0 (off0 + k) (len0 - k),
                      Text arr1 (off1 + k) (len1 - k))
      | otherwise = Nothing
      where
        a = A.unsafeIndex arr0 (off0 + i)
        b = A.unsafeIndex arr1 (off1 + i)
        isLeader = word8ToInt8 a >= -64
        k = if isLeader then i else j
{-# INLINE commonPrefixes #-}

-- | /O(n)/ Return the prefix of the second string if its suffix
-- matches the entire first string.
--
-- Examples:
--
-- >>> stripSuffix "bar" "foobar"
-- Just "foo"
--
-- >>> stripSuffix ""    "baz"
-- Just "baz"
--
-- >>> stripSuffix "foo" "quux"
-- Nothing
--
-- This is particularly useful with the @ViewPatterns@ extension to
-- GHC, as follows:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > import Data.Text as T
-- >
-- > quuxLength :: Text -> Int
-- > quuxLength (stripSuffix "quux" -> Just pre) = T.length pre
-- > quuxLength _                                = -1
stripSuffix :: Text -> Text -> Maybe Text
stripSuffix p@(Text _arr _off plen) t@(Text arr off len)
    | p `isSuffixOf` t = Just $! text arr off (len-plen)
    | otherwise        = Nothing

-- | Add a list of non-negative numbers.  Errors out on overflow.
sumP :: String -> [Int] -> Int
sumP fun = L.foldl' add 0
  where add a x
            | ax >= 0   = ax
            | otherwise = overflowError fun
          where ax = a + x
{-# INLINE sumP #-} -- Use foldl' and inline for fusion.

emptyError :: HasCallStack => String -> a
emptyError fun = P.error $ "Data.Text." ++ fun ++ ": empty input"

overflowError :: HasCallStack => String -> a
overflowError fun = P.error $ "Data.Text." ++ fun ++ ": size overflow"

-- | /O(n)/ Make a distinct copy of the given string, sharing no
-- storage with the original string.
--
-- As an example, suppose you read a large string, of which you need
-- only a small portion.  If you do not use 'copy', the entire original
-- array will be kept alive in memory by the smaller string. Making a
-- copy \"breaks the link\" to the original array, allowing it to be
-- garbage collected if there are no other live references to it.
copy :: Text -> Text
copy t@(Text arr off len)
  | null t = empty
  | otherwise = Text (A.run go) 0 len
  where
    go :: ST s (A.MArray s)
    go = do
      marr <- A.new len
      A.copyI len marr 0 arr off
      return marr

ord8 :: Char -> Word8
ord8 = P.fromIntegral . Char.ord

intToCSize :: Int -> CSize
intToCSize = P.fromIntegral

cSsizeToInt :: CSsize -> Int
cSsizeToInt = P.fromIntegral

word8ToInt8 :: Word8 -> Int8
word8ToInt8 = P.fromIntegral

-------------------------------------------------
-- NOTE: the named chunk below used by doctest;
--       verify the doctests via `doctest -fobject-code Data/Text.hs`

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as T
