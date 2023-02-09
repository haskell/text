{-# LANGUAGE BangPatterns, MagicHash, RankNTypes, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- |
-- Module      : Data.Text.Internal.Fusion.Common
-- Copyright   : (c) Bryan O'Sullivan 2009, 2012
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
-- This module provides a common stream fusion interface for text.
-- The stream interface allows us to write text pipelines which
-- do not allocate intermediate text values. For example, we could
-- guarantee no intermediate text is allocated by writing the following:
--
-- @
--   getNucleotides :: 'Data.Text.Internal.Text' -> 'Data.Text.Internal.Text'
--   getNucleotides =
--         'Data.Text.Internal.Fusion.unstream'
--       . 'filter' isNucleotide
--       . 'toLower'
--       . 'Data.Text.Internal.Fusion.stream'
--     where
--       isNucleotide chr =
--         chr == \'a\' ||
--         chr == \'c\' ||
--         chr == \'t\' ||
--         chr == \'g\'
-- @

module Data.Text.Internal.Fusion.Common
    (
    -- * Creation and elimination
      singleton
    , streamList
    , unstreamList
    , streamCString#

    -- * Basic interface
    , cons
    , snoc
    , append
    , head
    , uncons
    , last
    , tail
    , init
    , null
    , lengthI
    , compareLengthI
    , isSingleton

    -- * Transformations
    , map
    , intercalate
    , intersperse

    -- ** Case conversion
    -- $case
    , toCaseFold
    , toLower
    , toTitle
    , toUpper

    -- ** Justification
    , justifyLeftI

    -- * Folds
    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1

    -- ** Special folds
    , concat
    , concatMap
    , any
    , all
    , maximum
    , minimum

    -- * Construction
    -- ** Scans
    , scanl

    -- ** Generation and unfolding
    , replicateCharI
    , replicateI
    , unfoldr
    , unfoldrNI

    -- * Substrings
    -- ** Breaking strings
    , take
    , drop
    , takeWhile
    , dropWhile

    -- * Predicates
    , isPrefixOf

    -- * Searching
    , elem
    , filter

    -- * Indexing
    , findBy
    , indexI
    , findIndexI
    , countCharI

    -- * Zipping and unzipping
    , zipWith
    ) where

import Prelude (Bool(..), Char, Eq, (==), Int, Integral, Maybe(..),
                Ord(..), Ordering(..), String, (.), ($), (+), (-), (*), (++),
                (&&), fromIntegral, otherwise)
import qualified Data.List as L hiding (head, tail)
import qualified Prelude as P
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Char (isLetter, isSpace)
import GHC.Int (Int64(..))
import Data.Text.Internal.Encoding.Utf8 (chr2, chr3, chr4, utf8LengthByLeader)
import Data.Text.Internal.Fusion.Types
import Data.Text.Internal.Fusion.CaseMapping (foldMapping, lowerMapping, titleMapping,
                                     upperMapping)
import Data.Text.Internal.Fusion.Size
import GHC.Exts (Char(..), Char#, chr#)
import GHC.Prim (Addr#, indexWord8OffAddr#)
import GHC.Stack (HasCallStack)
import GHC.Types (Int(..))
import Data.Text.Internal.Unsafe.Char (unsafeChr8)
import GHC.Word

-- | /O(1)/ Convert a character into a 'Stream'
--
-- __Properties__
--
-- @'Data.Text.Internal.Fusion.unstream' . 'singleton' = 'Data.Text.singleton'@
singleton :: Char -> Stream Char
singleton c = Stream next False (codePointsSize 1)
    where next False = Yield c True
          next True  = Done
{-# INLINE [0] singleton #-}

-- | /O(n)/ Convert a list into a 'Stream'.
--
-- __Properties__
--
-- @'Data.Text.Internal.Fusion.unstream' . 'streamList' = 'Data.Text.pack'@
streamList :: [a] -> Stream a
{-# INLINE [0] streamList #-}
streamList s  = Stream next s unknownSize
    where next []       = Done
          next (x:xs)   = Yield x xs

-- | /O(n)/ Convert a 'Stream' into a list.
--
-- __Properties__
--
-- @'unstreamList' . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.unpack'@
unstreamList :: Stream a -> [a]
unstreamList (Stream next s0 _len) = unfold s0
    where unfold !s = case next s of
                        Done       -> []
                        Skip s'    -> unfold s'
                        Yield x s' -> x : unfold s'
{-# INLINE [0] unstreamList #-}

{-# RULES "STREAM streamList/unstreamList fusion" forall s. streamList (unstreamList s) = s #-}

-- | Stream the UTF-8-like packed encoding used by GHC to represent
-- constant strings in generated code.
--
-- This encoding uses the byte sequence "\xc0\x80" to represent NUL,
-- and the string is NUL-terminated.
--
-- __Properties__
--
-- @ 'Data.Text.Internal.Fusion.unstream' . 'streamCString#' addr# = 'Data.Text.Show.unpackCString#' addr#@
streamCString# :: Addr# -> Stream Char
streamCString# addr = Stream step 0 unknownSize
  where
    step !i
        | b == 0    = Done
        | otherwise = Yield chr (i + l)
      where b = at# i
            l = utf8LengthByLeader b
            next n = at# (i+n)
            chr = case l of
              1 -> unsafeChr8 b
              2 -> chr2 b (next 1)
              3 -> chr3 b (next 1) (next 2)
              _ -> chr4 b (next 1) (next 2) (next 3)
    at# (I# i#) = W8# (indexWord8OffAddr# addr i#)
{-# INLINE [0] streamCString# #-}

-- ----------------------------------------------------------------------------
-- * Basic stream functions

data C s = C0 !s
         | C1 !s

-- | /O(n)/ Adds a character to the front of a Stream Char.
--
-- __Properties__
--
-- @ 'Data.Text.Internal.Fusion.unstream' . 'cons' c . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.cons' c @
cons :: Char -> Stream Char -> Stream Char
cons !w (Stream next0 s0 len) = Stream next (C1 s0) (len + codePointsSize 1)
    where
      next (C1 s) = Yield w (C0 s)
      next (C0 s) = case next0 s of
                          Done -> Done
                          Skip s' -> Skip (C0 s')
                          Yield x s' -> Yield x (C0 s')
{-# INLINE [0] cons #-}

data Snoc a = N
            | J !a

-- | /O(n)/ Adds a character to the end of a stream.
--
-- __Properties__
--
-- @ 'Data.Text.Internal.Fusion.unstream' . 'snoc' c . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.snoc' c @
snoc :: Stream Char -> Char -> Stream Char
snoc (Stream next0 xs0 len) w = Stream next (J xs0) (len + codePointsSize 1)
  where
    next (J xs) = case next0 xs of
      Done        -> Yield w N
      Skip xs'    -> Skip    (J xs')
      Yield x xs' -> Yield x (J xs')
    next N = Done
{-# INLINE [0] snoc #-}

data E l r = L !l
           | R !r

-- | /O(n)/ Appends one Stream to the other.
--
-- __Properties__
--
-- @ 'Data.Text.Internal.Fusion.unstream' ('append' ('Data.Text.Internal.Fusion.stream' t1) ('Data.Text.Internal.Fusion.stream' t2)) = 'Data.Text.append' t1 t2@
append :: Stream Char -> Stream Char -> Stream Char
append (Stream next0 s01 len1) (Stream next1 s02 len2) =
    Stream next (L s01) (len1 + len2)
    where
      next (L s1) = case next0 s1 of
                         Done        -> Skip    (R s02)
                         Skip s1'    -> Skip    (L s1')
                         Yield x s1' -> Yield x (L s1')
      next (R s2) = case next1 s2 of
                          Done        -> Done
                          Skip s2'    -> Skip    (R s2')
                          Yield x s2' -> Yield x (R s2')
{-# INLINE [0] append #-}

-- | /O(1)/ Returns the first character of a 'Stream' 'Char', which must be non-empty.
-- This is a partial function, consider using 'uncons'.
--
-- __Properties__
--
-- @ 'head' . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.head' @
head :: HasCallStack => Stream Char -> Char
head (Stream next s0 _len) = loop_head s0
    where
      loop_head !s = case next s of
                      Yield x _ -> x
                      Skip s'   -> loop_head s'
                      Done      -> head_empty
{-# INLINE [0] head #-}

head_empty :: HasCallStack => a
head_empty = streamError "head" "Empty stream"
{-# NOINLINE head_empty #-}

-- | /O(1)/ Returns the first character and remainder of a 'Stream'
-- 'Char', or 'Nothing' if empty.
--
-- __Properties__
--
-- @ 'Data.Functor.fmap' 'Data.Tuple.fst' . 'uncons' . 'Data.Text.Internal.Fusion.stream' = 'Data.Functor.fmap' 'Data.Tuple.fst' . 'Data.Text.uncons' @
--
-- @ 'Data.Functor.fmap' ('Data.Text.Internal.Fusion.unstream' . 'Data.Tuple.snd') . 'uncons' . 'Data.Text.Internal.Fusion.stream' = 'Data.Functor.fmap' 'Data.Tuple.snd' . 'Data.Text.uncons' @
uncons :: Stream Char -> Maybe (Char, Stream Char)
uncons (Stream next s0 len) = loop_uncons s0
    where
      loop_uncons !s = case next s of
                         Yield x s1 -> Just (x, Stream next s1 (len - codePointsSize 1))
                         Skip s'    -> loop_uncons s'
                         Done       -> Nothing
{-# INLINE [0] uncons #-}

-- | /O(n)/ Returns the last character of a 'Stream' 'Char', which must
-- be non-empty.
--
-- __Properties__
--
-- @ 'last' . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.last' @
last :: HasCallStack => Stream Char -> Char
last (Stream next s0 _len) = loop0_last s0
    where
      loop0_last !s = case next s of
                        Done       -> emptyError "last"
                        Skip s'    -> loop0_last  s'
                        Yield x s' -> loop_last x s'
      loop_last !x !s = case next s of
                         Done        -> x
                         Skip s'     -> loop_last x  s'
                         Yield x' s' -> loop_last x' s'
{-# INLINE[0] last #-}

-- | /O(1)/ Returns all characters after the head of a 'Stream' 'Char', which must
-- be non-empty. This is a partial function, consider using 'uncons'.
--
-- __Properties__
--
-- @ 'Data.Text.Internal.unstream' . 'tail' . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.tail' @
tail :: HasCallStack => Stream Char -> Stream Char
tail (Stream next0 s0 len) = Stream next (C0 s0) (len - codePointsSize 1)
    where
      next (C0 s) = case next0 s of
                      Done       -> emptyError "tail"
                      Skip s'    -> Skip (C0 s')
                      Yield _ s' -> Skip (C1 s')
      next (C1 s) = case next0 s of
                      Done       -> Done
                      Skip s'    -> Skip    (C1 s')
                      Yield x s' -> Yield x (C1 s')
{-# INLINE [0] tail #-}

data Init s = Init0 !s
            | Init1 {-# UNPACK #-} !Char !s

-- | /O(1)/ Returns all but the last character of a 'Stream' 'Char', which
-- must be non-empty.
--
-- __Properties__
--
-- @ 'Data.Text.Internal.unstream' . 'init' . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.init' @
init :: HasCallStack => Stream Char -> Stream Char
init (Stream next0 s0 len) = Stream next (Init0 s0) (len - codePointsSize 1)
    where
      next (Init0 s) = case next0 s of
                         Done       -> emptyError "init"
                         Skip s'    -> Skip (Init0 s')
                         Yield x s' -> Skip (Init1 x s')
      next (Init1 x s)  = case next0 s of
                            Done        -> Done
                            Skip s'     -> Skip    (Init1 x s')
                            Yield x' s' -> Yield x (Init1 x' s')
{-# INLINE [0] init #-}

-- | /O(1)/ Tests whether a 'Stream' 'Char' is empty or not.
--
-- __Properties__
--
-- @ 'null' . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.null' @
null :: Stream Char -> Bool
null (Stream next s0 _len) = loop_null s0
    where
      loop_null !s = case next s of
                       Done      -> True
                       Yield _ _ -> False
                       Skip s'   -> loop_null s'
{-# INLINE[0] null #-}

-- | /O(n)/ Returns the number of characters in a string.
lengthI :: Integral a => Stream Char -> a
lengthI (Stream next s0 _len) = loop_length 0 s0
    where
      loop_length !z s  = case next s of
                           Done       -> z
                           Skip    s' -> loop_length z s'
                           Yield _ s' -> loop_length (z + 1) s'
{-# INLINE[0] lengthI #-}

-- | /O(n)/ Compares the count of characters in a string to a number.
--
-- This function gives the same answer as comparing against the result
-- of 'lengthI', but can short circuit if the count of characters is
-- greater than the number or if the stream can't possibly be as long
-- as the number supplied, and hence be more efficient.
compareLengthI :: Integral a => Stream Char -> a -> Ordering
compareLengthI (Stream next s0 len) n
    -- Note that @len@ tracks code units whereas we want to compare the length
    -- in code points. Specifically, a stream with hint @len@ may consist of
    -- anywhere from @len/2@ to @len@ code points.
  | n < 0 = GT
  | Just r <- compareSize len n' = r
  | otherwise = loop_cmp 0 s0
    where
      n' = codePointsSize $ fromIntegral n
      loop_cmp !z s  = case next s of
                         Done       -> compare z n
                         Skip    s' -> loop_cmp z s'
                         Yield _ s' | z > n     -> GT
                                    | otherwise -> loop_cmp (z + 1) s'
{-# INLINE[0] compareLengthI #-}

-- | /O(n)/ Indicate whether a string contains exactly one element.
--
-- __Properties__
--
-- @ 'isSingleton' . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.isSingleton' @
isSingleton :: Stream Char -> Bool
isSingleton (Stream next s0 _len) = loop 0 s0
    where
      loop !z s  = case next s of
                     Done            -> z == (1::Int)
                     Skip    s'      -> loop z s'
                     Yield _ s'
                         | z >= 1    -> False
                         | otherwise -> loop (z+1) s'
{-# INLINE[0] isSingleton #-}

-- ----------------------------------------------------------------------------
-- * Stream transformations

-- | /O(n)/ 'map' @f @xs is the 'Stream' 'Char' obtained by applying @f@
-- to each element of @xs@.
--
-- __Properties__
--
-- @ 'Data.Text.Internal.unstream' . 'map' f . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.map' f @
map :: (Char -> Char) -> Stream Char -> Stream Char
map f (Stream next0 s0 len) = Stream next s0 len
    where
      next !s = case next0 s of
                  Done       -> Done
                  Skip s'    -> Skip s'
                  Yield x s' -> Yield (f x) s'
{-# INLINE [0] map #-}

{-#
  RULES "STREAM map/map fusion" forall f g s.
     map f (map g s) = map (\x -> f (g x)) s
 #-}

data I s = I1 !s
         | I2 !s {-# UNPACK #-} !Char
         | I3 !s

-- | /O(n)/ Take a character and place it between each of the
-- characters of a 'Stream Char'.
--
-- __Properties__
--
-- @ 'Data.Text.Internal.unstream' . 'intersperse' c . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.intersperse' c @
intersperse :: Char -> Stream Char -> Stream Char
intersperse c (Stream next0 s0 len) = Stream next (I1 s0) (len + unknownSize)
    where
      next (I1 s) = case next0 s of
        Done       -> Done
        Skip s'    -> Skip (I1 s')
        Yield x s' -> Skip (I2 s' x)
      next (I2 s x)  = Yield x (I3 s)
      next (I3 s) = case next0 s of
        Done       -> Done
        Skip s'    -> Skip    (I3 s')
        Yield x s' -> Yield c (I2 s' x)
{-# INLINE [0] intersperse #-}

-- ----------------------------------------------------------------------------
-- ** Case conversions (folds)

-- $case
--
-- With Unicode text, it is incorrect to use combinators like @map
-- toUpper@ to case convert each character of a string individually.
-- Instead, use the whole-string case conversion functions from this
-- module.  For correctness in different writing systems, these
-- functions may map one input character to two or three output
-- characters.

-- | Map a 'Stream' through the given case-mapping function.
caseConvert :: (Char# -> _ {- unboxed Int64 -})
            -> Stream Char -> Stream Char
caseConvert remap (Stream next0 s0 len) =
    Stream next (CC s0 0) (len `unionSize` (3*len))
  where
    next (CC s 0) =
        case next0 s of
          Done       -> Done
          Skip s'    -> Skip (CC s' 0)
          Yield c@(C# c#) s' -> case I64# (remap c#) of
            0 -> Yield c (CC s' 0)
            ab -> let (a, b) = chopOffChar ab in
              Yield a (CC s' b)
    next (CC s ab) = let (a, b) = chopOffChar ab in Yield a (CC s b)

chopOffChar :: Int64 -> (Char, Int64)
chopOffChar ab = (chr a, ab `shiftR` 21)
  where
    chr (I# n) = C# (chr# n)
    mask = (1 `shiftL` 21) - 1
    a = fromIntegral $ ab .&. mask

-- | /O(n)/ Convert a string to folded case.  This function is mainly
-- useful for performing caseless (or case insensitive) string
-- comparisons.
--
-- A string @x@ is a caseless match for a string @y@ if and only if:
--
-- @'toCaseFold' x == 'toCaseFold' y@
--
-- The result string may be longer than the input string, and may
-- differ from applying 'toLower' to the input string.  For instance,
-- the Armenian small ligature men now (U+FB13) is case folded to the
-- bigram men now (U+0574 U+0576), while the micro sign (U+00B5) is
-- case folded to the Greek small letter letter mu (U+03BC) instead of
-- itself.
toCaseFold :: Stream Char -> Stream Char
toCaseFold = caseConvert foldMapping
{-# INLINE [0] toCaseFold #-}

-- | /O(n)/ Convert a string to upper case, using simple case
-- conversion.  The result string may be longer than the input string.
-- For instance, the German eszett (U+00DF) maps to the two-letter
-- sequence SS.
--
-- __Properties__
--
-- @ 'Data.Text.Internal.unstream' . 'toUpper' . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.toUpper' @
toUpper :: Stream Char -> Stream Char
toUpper = caseConvert upperMapping
{-# INLINE [0] toUpper #-}

-- | /O(n)/ Convert a string to lower case, using simple case
-- conversion.  The result string may be longer than the input string.
-- For instance, the Latin capital letter I with dot above (U+0130)
-- maps to the sequence Latin small letter i (U+0069) followed by
-- combining dot above (U+0307).
--
-- __Properties__
--
-- @ 'Data.Text.Internal.unstream' . 'toLower' . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.toLower' @
toLower :: Stream Char -> Stream Char
toLower = caseConvert lowerMapping
{-# INLINE [0] toLower #-}

-- | /O(n)/ Convert a string to title case, using simple case
-- conversion.
--
-- The first letter (as determined by 'Data.Char.isLetter')
-- of the input is converted to title case, as is
-- every subsequent letter that immediately follows a non-letter.
-- Every letter that immediately follows another letter is converted
-- to lower case.
--
-- The result string may be longer than the input string. For example,
-- the Latin small ligature &#xfb02; (U+FB02) is converted to the
-- sequence Latin capital letter F (U+0046) followed by Latin small
-- letter l (U+006C).
--
-- This function is not idempotent.
-- Consider lower-case letter @ŉ@ (U+0149 LATIN SMALL LETTER N PRECEDED BY APOSTROPHE).
-- Then 'T.toTitle' @"ŉ"@ = @"ʼN"@: the first (and the only) letter of the input
-- is converted to title case, becoming two letters.
-- Now @ʼ@ (U+02BC MODIFIER LETTER APOSTROPHE) is a modifier letter
-- and as such is recognised as a letter by 'Data.Char.isLetter',
-- so 'T.toTitle' @"ʼN"@ = @"'n"@.
--
-- /Note/: this function does not take language or culture specific
-- rules into account. For instance, in English, different style
-- guides disagree on whether the book name \"The Hill of the Red
-- Fox\" is correctly title cased&#x2014;but this function will
-- capitalize /every/ word.
--
-- __Properties__
--
-- @ 'Data.Text.Internal.unstream' . 'toTitle' . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.toTitle' @
toTitle :: Stream Char -> Stream Char
toTitle (Stream next0 s0 len) = Stream next (CC (False :*: s0) 0) (len + unknownSize)
  where
    next (CC (letter :*: s) 0) =
      case next0 s of
        Done            -> Done
        Skip s'         -> Skip (CC (letter :*: s') 0)
        Yield c@(C# c#) s'
          | nonSpace, letter -> case I64# (lowerMapping c#) of
            0 -> Yield c (CC (nonSpace :*: s') 0)
            ab -> let (a, b) = chopOffChar ab in
              Yield a (CC (nonSpace :*: s') b)
          | nonSpace    ->  case I64# (titleMapping c#) of
            0 -> Yield c (CC (letter' :*: s') 0)
            ab -> let (a, b) = chopOffChar ab in
              Yield a (CC (letter' :*: s') b)
          | otherwise   -> Yield c (CC (letter' :*: s') 0)
          where nonSpace = P.not (isSpace c)
                letter'  = isLetter c
    next (CC s ab) = let (a, b) = chopOffChar ab in Yield a (CC s b)
{-# INLINE [0] toTitle #-}

data Justify i s = Just1 !i !s
                 | Just2 !i !s

justifyLeftI :: Integral a => a -> Char -> Stream Char -> Stream Char
justifyLeftI k c (Stream next0 s0 len) =
    Stream next (Just1 0 s0) (larger (fromIntegral k * charSize c + len) len)
  where
    next (Just1 n s) =
        case next0 s of
          Done       -> next (Just2 n s)
          Skip s'    -> Skip (Just1 n s')
          Yield x s' -> Yield x (Just1 (n+1) s')
    next (Just2 n s)
        | n < k       = Yield c (Just2 (n+1) s)
        | otherwise   = Done
    {-# INLINE next #-}
{-# INLINE [0] justifyLeftI #-}

-- ----------------------------------------------------------------------------
-- * Reducing Streams (folds)

-- | foldl, applied to a binary operator, a starting value (typically the
-- left-identity of the operator), and a 'Stream', reduces the 'Stream' using the
-- binary operator, from left to right.
--
-- __Properties__
--
-- @ 'foldl' f z0 . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.foldl' f z0 @
foldl :: (b -> Char -> b) -> b -> Stream Char -> b
foldl f z0 (Stream next s0 _len) = loop_foldl z0 s0
    where
      loop_foldl z !s = case next s of
                          Done -> z
                          Skip s' -> loop_foldl z s'
                          Yield x s' -> loop_foldl (f z x) s'
{-# INLINE [0] foldl #-}

-- | A strict version of foldl.
--
-- __Properties__
--
-- @ 'foldl'' f z0 . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.foldl'' f z0 @
foldl' :: (b -> Char -> b) -> b -> Stream Char -> b
foldl' f z0 (Stream next s0 _len) = loop_foldl' z0 s0
    where
      loop_foldl' !z !s = case next s of
                            Done -> z
                            Skip s' -> loop_foldl' z s'
                            Yield x s' -> loop_foldl' (f z x) s'
{-# INLINE [0] foldl' #-}

-- | foldl1 is a variant of foldl that has no starting value argument,
-- and thus must be applied to non-empty Streams.
--
-- __Properties__
--
-- @ 'foldl1' f . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.foldl1' f @
foldl1 :: HasCallStack => (Char -> Char -> Char) -> Stream Char -> Char
foldl1 f (Stream next s0 _len) = loop0_foldl1 s0
    where
      loop0_foldl1 !s = case next s of
                          Skip s' -> loop0_foldl1 s'
                          Yield x s' -> loop_foldl1 x s'
                          Done -> emptyError "foldl1"
      loop_foldl1 z !s = case next s of
                           Done -> z
                           Skip s' -> loop_foldl1 z s'
                           Yield x s' -> loop_foldl1 (f z x) s'
{-# INLINE [0] foldl1 #-}

-- | A strict version of foldl1.
--
-- __Properties__
--
-- @ 'foldl1'' f . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.foldl1'' f @
foldl1' :: HasCallStack => (Char -> Char -> Char) -> Stream Char -> Char
foldl1' f (Stream next s0 _len) = loop0_foldl1' s0
    where
      loop0_foldl1' !s = case next s of
                           Skip s' -> loop0_foldl1' s'
                           Yield x s' -> loop_foldl1' x s'
                           Done -> emptyError "foldl1"
      loop_foldl1' !z !s = case next s of
                             Done -> z
                             Skip s' -> loop_foldl1' z s'
                             Yield x s' -> loop_foldl1' (f z x) s'
{-# INLINE [0] foldl1' #-}

-- | 'foldr', applied to a binary operator, a starting value (typically the
-- right-identity of the operator), and a stream, reduces the stream using the
-- binary operator, from right to left.
--
-- __Properties__
--
-- @ 'foldr' f z0 . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.foldr' f z0 @
foldr :: (Char -> b -> b) -> b -> Stream Char -> b
foldr f z (Stream next s0 _len) = loop_foldr s0
    where
      loop_foldr !s = case next s of
                        Done -> z
                        Skip s' -> loop_foldr s'
                        Yield x s' -> f x (loop_foldr s')
{-# INLINE [0] foldr #-}

-- | foldr1 is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty streams.
--
-- __Properties__
--
-- @ 'foldr1' f . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.foldr1' f @
foldr1 :: HasCallStack => (Char -> Char -> Char) -> Stream Char -> Char
foldr1 f (Stream next s0 _len) = loop0_foldr1 s0
  where
    loop0_foldr1 !s = case next s of
      Done       -> emptyError "foldr1"
      Skip    s' -> loop0_foldr1  s'
      Yield x s' -> loop_foldr1 x s'

    loop_foldr1 x !s = case next s of
      Done        -> x
      Skip     s' -> loop_foldr1 x s'
      Yield x' s' -> f x (loop_foldr1 x' s')
{-# INLINE [0] foldr1 #-}

-- | intercalate str strs inserts the stream str in between the streams strs and
-- concatenates the result.
--
-- __Properties__
--
-- @ 'intercalate' s = 'concat' . 'L.intersperse' s @
intercalate :: Stream Char -> [Stream Char] -> Stream Char
intercalate s = concat . (L.intersperse s)
{-# INLINE [0] intercalate #-}

-- ----------------------------------------------------------------------------
-- ** Special folds

-- | /O(n)/ Concatenate a list of streams.
--
-- __Properties__
--
-- @'Data.Text.Internal.Fusion.unstream' . 'concat' . 'Data.Functor.fmap' 'Data.Text.Internal.Fusion.stream'  = 'Data.Text.concat'@
concat :: [Stream Char] -> Stream Char
concat = L.foldr append empty
{-# INLINE [0] concat #-}

-- | Map a function over a stream that results in a stream and concatenate the
-- results.
--
-- __Properties__
--
-- @'Data.Text.Internal.Fusion.unstream' . 'concatMap' ('Data.Text.Fusion.stream' . f) . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.concatMap' f@
concatMap :: (Char -> Stream Char) -> Stream Char -> Stream Char
concatMap f = foldr (append . f) empty
{-# INLINE [0] concatMap #-}

-- | /O(n)/ any @p @xs determines if any character in the stream
-- @xs@ satisfies the predicate @p@.
--
-- __Properties__
--
-- @'any' f . 'Data.Text.Fusion.stream' = 'Data.Text.any' f@
any :: (Char -> Bool) -> Stream Char -> Bool
any p (Stream next0 s0 _len) = loop_any s0
    where
      loop_any !s = case next0 s of
                      Done                   -> False
                      Skip s'                -> loop_any s'
                      Yield x s' | p x       -> True
                                 | otherwise -> loop_any s'
{-# INLINE [0] any #-}

-- | /O(n)/ all @p @xs determines if all characters in the 'Text'
-- @xs@ satisfy the predicate @p@.
--
-- __Properties__
--
-- @'all' f . 'Data.Text.Fusion.stream' = 'Data.Text.all' f@
all :: (Char -> Bool) -> Stream Char -> Bool
all p (Stream next0 s0 _len) = loop_all s0
    where
      loop_all !s = case next0 s of
                      Done                   -> True
                      Skip s'                -> loop_all s'
                      Yield x s' | p x       -> loop_all s'
                                 | otherwise -> False
{-# INLINE [0] all #-}

-- | /O(n)/ maximum returns the maximum value from a stream, which must be
-- non-empty.
--
-- __Properties__
--
-- @'maximum' . 'Data.Text.Fusion.stream' = 'Data.Text.maximum'@
maximum :: HasCallStack => Stream Char -> Char
maximum (Stream next0 s0 _len) = loop0_maximum s0
    where
      loop0_maximum !s   = case next0 s of
                             Done       -> emptyError "maximum"
                             Skip s'    -> loop0_maximum s'
                             Yield x s' -> loop_maximum x s'
      loop_maximum !z !s = case next0 s of
                             Done            -> z
                             Skip s'         -> loop_maximum z s'
                             Yield x s'
                                 | x > z     -> loop_maximum x s'
                                 | otherwise -> loop_maximum z s'
{-# INLINE [0] maximum #-}

-- | /O(n)/ minimum returns the minimum value from a 'Text', which must be
-- non-empty.
--
-- __Properties__
--
-- @'minimum' . 'Data.Text.Fusion.stream' = 'Data.Text.minimum'@
minimum :: HasCallStack => Stream Char -> Char
minimum (Stream next0 s0 _len) = loop0_minimum s0
    where
      loop0_minimum !s   = case next0 s of
                             Done       -> emptyError "minimum"
                             Skip s'    -> loop0_minimum s'
                             Yield x s' -> loop_minimum x s'
      loop_minimum !z !s = case next0 s of
                             Done            -> z
                             Skip s'         -> loop_minimum z s'
                             Yield x s'
                                 | x < z     -> loop_minimum x s'
                                 | otherwise -> loop_minimum z s'
{-# INLINE [0] minimum #-}

-- -----------------------------------------------------------------------------
-- * Building streams
--
-- | /O(n)/ 'scanl' is similar to 'foldl', but returns a stream of
-- successive reduced values from the left. Conceptually, if we
-- write the input stream as a list then we have:
--
-- > scanl f z [x1, x2, ...] == [z, z 'f' x1, (z 'f' x1) 'f' x2, ...]
--
-- __Properties__
--
-- @'head' ('scanl' f z xs) = z@
--
-- @'last' ('scanl' f z xs) = 'foldl' f z xs@
scanl :: (Char -> Char -> Char) -> Char -> Stream Char -> Stream Char
scanl f z0 (Stream next0 s0 len) = Stream next (Scan1 z0 s0) (len+1) -- HINT maybe too low
  where
    {-# INLINE next #-}
    next (Scan1 z s) = Yield z (Scan2 z s)
    next (Scan2 z s) = case next0 s of
                         Yield x s' -> let !x' = f z x
                                       in Yield x' (Scan2 x' s')
                         Skip s'    -> Skip (Scan2 z s')
                         Done       -> Done
{-# INLINE [0] scanl #-}

-- -----------------------------------------------------------------------------
-- ** Generating and unfolding streams

-- | /O(n)/ 'replicateCharI' @n@ @c@ is a 'Stream' 'Char' of length @n@ with @c@ the
-- value of every element.
replicateCharI :: Integral a => a -> Char -> Stream Char
replicateCharI !n !c
    | n < 0     = empty
    | otherwise = Stream next 0 (fromIntegral n) -- HINT maybe too low
  where
    next !i | i >= n    = Done
            | otherwise = Yield c (i + 1)
{-# INLINE [0] replicateCharI #-}

data RI s = RI !s {-# UNPACK #-} !Int64


-- | /O(n*m)/ 'replicateI' @n@ @t@ is a 'Stream' 'Char' consisting of the input
-- @t@ repeated @n@ times.
replicateI :: Int64 -> Stream Char -> Stream Char
replicateI n (Stream next0 s0 len) =
    Stream next (RI s0 0) (int64ToSize (max 0 n) * len)
  where
    next (RI s k)
        | k >= n = Done
        | otherwise = case next0 s of
                        Done       -> Skip    (RI s0 (k+1))
                        Skip s'    -> Skip    (RI s' k)
                        Yield x s' -> Yield x (RI s' k)
{-# INLINE [0] replicateI #-}

-- | /O(n)/, where @n@ is the length of the result. The unfoldr function
-- is analogous to the List 'unfoldr'. unfoldr builds a stream
-- from a seed value. The function takes the element and returns
-- Nothing if it is done producing the stream or returns Just
-- (a,b), in which case, a is the next Char in the string, and b is
-- the seed value for further production.
--
-- __Properties__
--
-- @'Data.Text.Internal.Fusion.unstream' . 'unfoldr' f z = 'Data.Text.unfoldr' f z@
unfoldr :: (a -> Maybe (Char,a)) -> a -> Stream Char
unfoldr f s0 = Stream next s0 unknownSize
    where
      {-# INLINE next #-}
      next !s = case f s of
                 Nothing      -> Done
                 Just (w, s') -> Yield w s'
{-# INLINE [0] unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrNI' builds a stream from a seed
-- value. However, the length of the result is limited by the
-- first argument to 'unfoldrNI'. This function is more efficient than
-- 'unfoldr' when the length of the result is known.
--
-- __Properties__
--
-- @'Data.Text.Internal.Fusion.unstream' ('unfoldrNI' n f z) = 'Data.Text.unfoldrN' n f z@
unfoldrNI :: Integral a => a -> (b -> Maybe (Char,b)) -> b -> Stream Char
unfoldrNI n f s0 | n <  0    = empty
                 | otherwise = Stream next (0 :*: s0) (maxSize $ fromIntegral (n*2))
    where
      {-# INLINE next #-}
      next (z :*: s) = case f s of
          Nothing                  -> Done
          Just (w, s') | z >= n    -> Done
                       | otherwise -> Yield w ((z + 1) :*: s')
{-# INLINE unfoldrNI #-}

-------------------------------------------------------------------------------
--  * Substreams

-- | /O(n)/ @'take' n@, applied to a stream, returns the prefix of the
-- stream of length @n@, or the stream itself if @n@ is greater than the
-- length of the stream.
--
-- __Properties__
--
-- @'Data.Text.Internal.Fusion.unstream' . 'take' n . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.take' n@
take :: Integral a => a -> Stream Char -> Stream Char
take n0 (Stream next0 s0 len) =
    Stream next (n0' :*: s0) (smaller len (codePointsSize $ fromIntegral n0'))
    where
      n0' = max n0 0

      {-# INLINE next #-}
      next (n :*: s) | n <= 0    = Done
                     | otherwise = case next0 s of
                                     Done -> Done
                                     Skip s' -> Skip (n :*: s')
                                     Yield x s' -> Yield x ((n-1) :*: s')
{-# INLINE [0] take #-}

data Drop a s = NS !s
              | JS !a !s

-- | /O(n)/ @'drop' n@, applied to a stream, returns the suffix of the
-- stream after the first @n@ characters, or the empty stream if @n@
-- is greater than the length of the stream.
--
-- __Properties__
--
-- @'Data.Text.Internal.Fusion.unstream' . 'drop' n . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.drop' n@
drop :: Integral a => a -> Stream Char -> Stream Char
drop n0 (Stream next0 s0 len) =
    Stream next (JS n0' s0) (len - codePointsSize (fromIntegral n0'))
  where
    n0' = max n0 0

    {-# INLINE next #-}
    next (JS n s)
      | n <= 0    = Skip (NS s)
      | otherwise = case next0 s of
          Done       -> Done
          Skip    s' -> Skip (JS n    s')
          Yield _ s' -> Skip (JS (n-1) s')
    next (NS s) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (NS s')
      Yield x s' -> Yield x (NS s')
{-# INLINE [0] drop #-}

-- | 'takeWhile', applied to a predicate @p@ and a stream, returns the
-- longest prefix (possibly empty) of elements that satisfy @p@.
--
-- __Properties__
--
-- @'Data.Text.Internal.Fusion.unstream' . 'takeWhile' p . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.takeWhile' p@
takeWhile :: (Char -> Bool) -> Stream Char -> Stream Char
takeWhile p (Stream next0 s0 len) = Stream next s0 (len - unknownSize)
    where
      {-# INLINE next #-}
      next !s = case next0 s of
                  Done    -> Done
                  Skip s' -> Skip s'
                  Yield x s' | p x       -> Yield x s'
                             | otherwise -> Done
{-# INLINE [0] takeWhile #-}

-- | @'dropWhile' p xs@ returns the suffix remaining after @'takeWhile' p xs@.
--
-- __Properties__
--
-- @'Data.Text.Internal.Fusion.unstream' . 'dropWhile' p . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.dropWhile' p@
dropWhile :: (Char -> Bool) -> Stream Char -> Stream Char
dropWhile p (Stream next0 s0 len) = Stream next (L s0) (len - unknownSize)
    where
    {-# INLINE next #-}
    next (L s)  = case next0 s of
      Done                   -> Done
      Skip    s'             -> Skip    (L s')
      Yield x s' | p x       -> Skip    (L s')
                 | otherwise -> Yield x (R s')
    next (R s) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (R s')
      Yield x s' -> Yield x (R s')
{-# INLINE [0] dropWhile #-}

-- | /O(n)/ The 'isPrefixOf' function takes two 'Stream's and returns
-- 'True' if and only if the first is a prefix of the second.
--
-- __Properties__
--
-- @ 'isPrefixOf' ('Data.Text.Internal.Fusion.stream' t1) ('Data.Text.Internal.Fusion.stream' t2) = 'Data.Text.isPrefixOf' t1 t2@
isPrefixOf :: (Eq a) => Stream a -> Stream a -> Bool
isPrefixOf (Stream next1 s1 _) (Stream next2 s2 _) = loop (next1 s1) (next2 s2)
    where
      loop Done      _ = True
      loop _    Done = False
      loop (Skip s1')     (Skip s2')     = loop (next1 s1') (next2 s2')
      loop (Skip s1')     x2             = loop (next1 s1') x2
      loop x1             (Skip s2')     = loop x1          (next2 s2')
      loop (Yield x1 s1') (Yield x2 s2') = x1 == x2 &&
                                           loop (next1 s1') (next2 s2')
{-# INLINE [0] isPrefixOf #-}

-- ----------------------------------------------------------------------------
-- * Searching

-------------------------------------------------------------------------------
-- ** Searching by equality

-- | /O(n)/ 'elem' is the stream membership predicate.
--
-- __Properties__
--
-- @ 'elem' c . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.elem' c@
elem :: Char -> Stream Char -> Bool
elem w (Stream next s0 _len) = loop_elem s0
    where
      loop_elem !s = case next s of
                       Done -> False
                       Skip s' -> loop_elem s'
                       Yield x s' | x == w -> True
                                  | otherwise -> loop_elem s'
{-# INLINE [0] elem #-}

-------------------------------------------------------------------------------
-- ** Searching with a predicate

-- | /O(n)/ The 'findBy' function takes a predicate and a stream,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- __Properties__
--
-- @ 'findBy' p . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.find' p@
findBy :: (Char -> Bool) -> Stream Char -> Maybe Char
findBy p (Stream next s0 _len) = loop_find s0
    where
      loop_find !s = case next s of
                       Done -> Nothing
                       Skip s' -> loop_find s'
                       Yield x s' | p x -> Just x
                                  | otherwise -> loop_find s'
{-# INLINE [0] findBy #-}

-- | /O(n)/ Stream index (subscript) operator, starting from 0.
--
-- __Properties__
--
-- @ 'indexI' ('Data.Text.Internal.Fusion.stream' t) n = 'Data.Text.index' t n@
indexI :: (HasCallStack, Integral a) => Stream Char -> a -> Char
indexI (Stream next s0 _len) n0
  | n0 < 0    = streamError "index" "Negative index"
  | otherwise = loop_index n0 s0
  where
    loop_index !n !s = case next s of
      Done                   -> streamError "index" "Index too large"
      Skip    s'             -> loop_index  n    s'
      Yield x s' | n == 0    -> x
                 | otherwise -> loop_index (n-1) s'
{-# INLINE [0] indexI #-}

-- | /O(n)/ 'filter', applied to a predicate and a stream,
-- returns a stream containing those characters that satisfy the
-- predicate.
--
-- __Properties__
--
-- @ 'Data.Text.Internal.unstream' . 'filter' p . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.filter' p @
filter :: (Char -> Bool) -> Stream Char -> Stream Char
filter p (Stream next0 s0 len) =
    Stream next s0 (len - unknownSize) -- HINT maybe too high
  where
    next !s = case next0 s of
                Done                   -> Done
                Skip    s'             -> Skip    s'
                Yield x s' | p x       -> Yield x s'
                           | otherwise -> Skip    s'
{-# INLINE [0] filter #-}

{-# RULES
  "STREAM filter/filter fusion" forall p q s.
  filter p (filter q s) = filter (\x -> q x && p x) s
  #-}

-- | The 'findIndexI' function takes a predicate and a stream and
-- returns the index of the first element in the stream satisfying the
-- predicate.
--
-- __Properties__
--
-- @'findIndexI' p . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.findIndex' p @
findIndexI :: Integral a => (Char -> Bool) -> Stream Char -> Maybe a
findIndexI p s = case findIndicesI p s of
                  (i:_) -> Just i
                  _     -> Nothing
{-# INLINE [0] findIndexI #-}

-- | The 'findIndicesI' function takes a predicate and a stream and
-- returns all indices of the elements in the stream satisfying the
-- predicate.
findIndicesI :: Integral a => (Char -> Bool) -> Stream Char -> [a]
findIndicesI p (Stream next s0 _len) = loop_findIndex 0 s0
  where
    loop_findIndex !i !s = case next s of
      Done                   -> []
      Skip    s'             -> loop_findIndex i     s' -- hmm. not caught by QC
      Yield x s' | p x       -> i : loop_findIndex (i+1) s'
                 | otherwise -> loop_findIndex (i+1) s'
{-# INLINE [0] findIndicesI #-}

-------------------------------------------------------------------------------
-- * Zipping

-- | Strict triple.
data Zip a b m = Z1 !a !b
               | Z2 !a !b !m

-- | zipWith generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.
--
-- __Properties__
--
-- @ 'Data.Text.Internal.Fusion.unstream' ('zipWith' f ('Data.Text.Internal.Fusion.stream' t1) ('Data.Text.Internal.Fusion.stream' t2)) = 'Data.Text.zipWith' f t1 t2@
zipWith :: (a -> a -> b) -> Stream a -> Stream a -> Stream b
zipWith f (Stream next0 sa0 len1) (Stream next1 sb0 len2) =
    Stream next (Z1 sa0 sb0) (smaller len1 len2)
    where
      next (Z1 sa sb) = case next0 sa of
                          Done -> Done
                          Skip sa' -> Skip (Z1 sa' sb)
                          Yield a sa' -> Skip (Z2 sa' sb a)

      next (Z2 sa' sb a) = case next1 sb of
                             Done -> Done
                             Skip sb' -> Skip (Z2 sa' sb' a)
                             Yield b sb' -> Yield (f a b) (Z1 sa' sb')
{-# INLINE [0] zipWith #-}

-- | /O(n)/ The 'countCharI' function returns the number of times the
-- query element appears in the given stream.
--
-- __Properties__
--
-- @'countCharI' c . 'Data.Text.Internal.Fusion.stream' = 'Data.Text.countChar' c @
countCharI :: Integral a => Char -> Stream Char -> a
countCharI a (Stream next s0 _len) = loop 0 s0
  where
    loop !i !s = case next s of
      Done                   -> i
      Skip    s'             -> loop i s'
      Yield x s' | a == x    -> loop (i+1) s'
                 | otherwise -> loop i s'
{-# INLINE [0] countCharI #-}

streamError :: HasCallStack => String -> String -> a
streamError func msg = P.error $ "Data.Text.Internal.Fusion.Common." ++ func ++ ": " ++ msg

emptyError :: HasCallStack => String -> a
emptyError func = internalError func "Empty input"

internalError :: HasCallStack => String -> a
internalError func = streamError func "Internal error"

int64ToSize :: Int64 -> Size
int64ToSize = fromIntegral
