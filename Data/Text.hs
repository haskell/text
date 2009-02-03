{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Data.Text
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009,
--               (c) Duncan Coutts 2009
--
-- License     : BSD-style
-- Maintainer  : rtharper@aftereternity.co.uk, bos@serpentine.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- A time and space-efficient implementation of Unicode text using
-- packed Word16 arrays.  Suitable for performance critical use, both
-- in terms of large data quantities and high speed.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.Text as T

module Data.Text
    (
    -- * Fusion
    -- $fusion

    -- * Types
      Text

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
    , head
    , last
    , tail
    , init
    , null
    , length

    -- * Transformations
    , map
    , intercalate
    , intersperse
    , transpose
    , reverse

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
    -- , scanl1
    -- , scanr
    -- , scanr1

    -- ** Accumulating maps
    -- , mapAccumL
    -- , mapAccumR

    -- ** Generation and unfolding
    , replicate
    , unfoldr
    , unfoldrN

    -- * Substrings

    -- ** Breaking strings
    , take
    , drop
    , takeWhile
    , dropWhile
    -- , splitAt
    -- , span
    -- , break
    -- , group
    -- , groupBy
    , inits
    , tails

    -- ** Breaking into many substrings
    -- , split
    -- , splitWith
    -- , breakSubstring

    -- ** Breaking into lines and words
    , lines
    , words
    , unlines
    , unwords

    -- * Predicates
    -- , isPrefixOf
    -- , isSuffixOf
    -- , isInfixOf

    -- * Searching
    , elem
    , filter
    , find
    -- , partition

    -- , findSubstring
    -- , findSubstrings
    
    -- * Indexing
    , index
    , findIndex
    -- , findIndices
    , elemIndex
    -- , elemIndices
    -- , count

    -- * Zipping and unzipping
    , zipWith

    -- * Ordered ByteStrings
    , -- sort
    ) where

import Prelude (Char, Bool, Functor(..), Int, Maybe(..), String,
                Eq, (==), (++), error,
                Show, showsPrec,
                Read, readsPrec,
                (&&), (||), (+), (-), (<), (>), (<=), (>=), (.),
                return, otherwise)
import Data.Char (isSpace)
import Control.Monad.ST (ST)
import qualified Data.Text.Array as A
import qualified Data.List as L
import Data.Monoid (Monoid(..))
import Data.Word (Word16)
import Data.String (IsString(..))

import qualified Data.Text.Fusion as S
import Data.Text.Fusion (Stream(..), Step(..), stream, unstream)
import Data.Text.Internal (Text(..), empty)
import qualified Prelude as P
import Data.Text.UnsafeChar (unsafeChr)
import qualified Data.Text.Utf16 as U16

-- $fusion
--
-- Most of the functions in this module are subject to /array fusion/,
-- meaning that a pipeline of functions will usually allocate at most
-- one 'Text' value.

instance Eq Text where
    t1 == t2 = (stream t1) `S.eq` (stream t2)

instance Show Text where
    showsPrec p ps r = showsPrec p (unpack ps) r

instance Read Text where
    readsPrec p str = [(pack x,y) | (x,y) <- readsPrec p str]

instance Monoid Text where
    mempty  = empty
    mappend = append
    mconcat = concat

instance IsString Text where
    fromString = pack

-- -----------------------------------------------------------------------------
-- * Conversion to/from 'Text'

-- | /O(n)/ Convert a 'String' into a 'Text'.
--
-- This function is subject to array fusion.
pack :: String -> Text
pack str = (unstream (stream_list str))
    where
      stream_list s0 = S.Stream next s0 (P.length s0) -- total guess
          where
            next []     = S.Done
            next (x:xs) = S.Yield x xs
{-# INLINE [1] pack #-}
-- TODO: Has to do validation! -- No, it doesn't, the

-- | /O(n)/ Convert a Text into a String.
-- Subject to array fusion.
unpack :: Text -> String
unpack txt = (unstream_list (stream txt))
    where
      unstream_list (S.Stream next s0 _len) = unfold s0
          where
            unfold !s = case next s of
                          S.Done       -> []
                          S.Skip s'    -> unfold s'
                          S.Yield x s' -> x : unfold s'
{-# INLINE [1] unpack #-}

-- | /O(1)/ Convert a character into a Text.
-- Subject to array fusion.
singleton :: Char -> Text
singleton c = unstream (Stream next (c:[]) 1)
    where
      {-# INLINE next #-}
      next (k:ks) = Yield k ks
      next []     = Done
{-# INLINE [1] singleton #-}

-- -----------------------------------------------------------------------------
-- * Basic functions

-- | /O(n)/ Adds a character to the front of a 'Text'.  This function
-- is more costly than its 'List' counterpart because it requires
-- copying a new array.  Subject to array fusion.
cons :: Char -> Text -> Text
cons c t = unstream (S.cons c (stream t))
{-# INLINE cons #-}

-- | /O(n)/ Adds a character to the end of a 'Text'.  This copies the
-- entire array in the process.  Subject to array fusion.
snoc :: Text -> Char -> Text
snoc t c = unstream (S.snoc (stream t) c)
{-# INLINE snoc #-}

-- | /O(n)/ Appends one 'Text' to the other by copying both of them
-- into a new 'Text'.  Subject to array fusion.
append :: Text -> Text -> Text
append (Text arr1 off1 len1) (Text arr2 off2 len2) = Text (A.run x) 0 len
    where
      len = len1+len2
      x = do
        arr <- A.unsafeNew len :: ST s (A.MArray s Word16)
        copy arr1 off1 (len1+off1) arr 0
        copy arr2 off2 (len2+off2) arr len1
        return arr
            where
              copy arr i max arr' j
                  | i >= max  = return ()
                  | otherwise = do A.unsafeWrite arr' j (arr `A.unsafeIndex` i)
                                   copy arr (i+1) max arr' (j+1)
{-# INLINE append #-}

{-# RULES
"TEXT append -> fused" [~1] forall t1 t2.
    append t1 t2 = unstream (S.append (stream t1) (stream t2))
"TEXT append -> unfused" [1] forall t1 t2.
    unstream (S.append (stream t1) (stream t2)) = append t1 t2
 #-}

-- | /O(1)/ Returns the first character of a 'Text', which must be
-- non-empty.  Subject to array fusion.
head :: Text -> Char
head t = S.head (stream t)
{-# INLINE head #-}

-- | /O(1)/ Returns the first character and rest of a 'Text', or
-- 'Nothing' if empty. Subject to array fusion.
uncons :: Text -> Maybe (Char, Text)
uncons (Text arr off len)
    | len <= 0  = Nothing
    | otherwise = Just (c, Text arr (off+k) (len-k))
    where c | single    = unsafeChr m
            | otherwise = U16.chr2 m n
          k | single    = 1
            | otherwise = 2
          single        = m < 0xD800 || m > 0xDBFF
          m             = A.unsafeIndex arr off
          n             = A.unsafeIndex arr (off+1)
{-# INLINE uncons #-}

second :: (b -> c) -> (a,b) -> (a,c)
second f (a, b) = (a, f b)

{-# RULES
"TEXT uncons -> fused" [~1] forall t.
    uncons t = fmap (second unstream) (S.uncons (stream t))
"TEXT uncons -> unfused" [1] forall t.
    fmap (second unstream) (S.uncons (stream t)) = uncons t
  #-}

-- | /O(1)/ Returns the last character of a 'Text', which must be
-- non-empty.  Subject to array fusion.
last :: Text -> Char
last (Text arr off len)
    | len <= 0                 = errorEmptyList "last"
    | n < 0xDC00 || n > 0xDFFF = unsafeChr n
    | otherwise                = U16.chr2 n0 n
    where
      n  = A.unsafeIndex arr (off+len-1)
      n0 = A.unsafeIndex arr (off+len-2)
{-# INLINE [1] last #-}

{-# RULES
"TEXT last -> fused" [~1] forall t.
    last t = S.last (stream t)
"TEXT last -> unfused" [1] forall t.
    S.last (stream t) = last t
  #-}


-- | /O(1)/ Returns all characters after the head of a 'Text', which
-- must be non-empty.  Subject to array fusion.
tail :: Text -> Text
tail (Text arr off len)
    | len <= 0                   = errorEmptyList "tail"
    | n >= 0xD800 && n <= 0xDBFF = Text arr (off+2) (len-2)
    | otherwise                  = Text arr (off+1) (len-1)
    where
      n = A.unsafeIndex arr off
{-# INLINE [1] tail #-}



-- | /O(1)/ Returns all but the last character of a 'Text', which must
-- be non-empty.  Subject to array fusion.
init :: Text -> Text
init (Text arr off len) | len <= 0                   = errorEmptyList "init"
                        | n >= 0xDC00 && n <= 0xDFFF = Text arr off (len-2)
                        | otherwise                  = Text arr off (len-1)
    where
      n = A.unsafeIndex arr (off+len-1)
{-# INLINE [1] init #-}

{-# RULES
"TEXT init -> fused" [~1] forall t.
    init t = unstream (S.init (stream t))
"TEXT init -> unfused" [1] forall t.
    unstream (S.init (stream t)) = init t
 #-}

-- | /O(1)/ Tests whether a 'Text' is empty or not.  Subject to array
-- fusion.
null :: Text -> Bool
null t = S.null (stream t)
{-# INLINE null #-}

-- | /O(n)/ Returns the number of characters in a 'Text'.
-- Subject to array fusion.
length :: Text -> Int
length t = S.length (stream t)
{-# INLINE length #-}

-- -----------------------------------------------------------------------------
-- * Transformations
-- | /O(n)/ 'map' @f @xs is the 'Text' obtained by applying @f@ to
-- each element of @xs@.  Subject to array fusion.
map :: (Char -> Char) -> Text -> Text
map f t = unstream (S.map f (stream t))
{-# INLINE [1] map #-}

-- | /O(n)/ The 'intercalate' function takes a 'Text' and a list of
-- 'Text's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: Text -> [Text] -> Text
intercalate t ts = unstream (S.intercalate (stream t) (L.map stream ts))
{-# INLINE intercalate #-}

-- | /O(n)/ The 'intersperse' function takes a character and places it
-- between the characters of a 'Text'.  Subject to array fusion.
intersperse     :: Char -> Text -> Text
intersperse c t = unstream (S.intersperse c (stream t))
{-# INLINE intersperse #-}

-- | /O(n)/ Reverse the characters of a string. Subject to array fusion.
reverse :: Text -> Text
reverse t = S.reverse (stream t)
{-# INLINE reverse #-}

-- | /O(n)/ The 'transpose' function transposes the rows and columns
-- of its 'Text' argument.  Note that this function uses 'pack',
-- 'unpack', and the 'List' version of transpose and is thus not very
-- efficient.
transpose :: [Text] -> [Text]
transpose ts = P.map pack (L.transpose (P.map unpack ts))

-- -----------------------------------------------------------------------------
-- * Reducing 'Text's (folds)

-- | 'foldl', applied to a binary operator, a starting value
-- (typically the left-identity of the operator), and a 'Text',
-- reduces the 'Text' using the binary operator, from left to right.
-- Subject to array fusion.
foldl :: (b -> Char -> b) -> b -> Text -> b
foldl f z t = S.foldl f z (stream t)
{-# INLINE foldl #-}

-- | A strict version of 'foldl'.
-- Subject to array fusion.
foldl' :: (b -> Char -> b) -> b -> Text -> b
foldl' f z t = S.foldl' f z (stream t)
{-# INLINE foldl' #-}

-- | A variant of 'foldl' that has no starting value argument, and
-- thus must be applied to a non-empty 'Text'.  Subject to array
-- fusion.
foldl1 :: (Char -> Char -> Char) -> Text -> Char
foldl1 f t = S.foldl1 f (stream t)
{-# INLINE foldl1 #-}

-- | A strict version of 'foldl1'.
-- Subject to array fusion.
foldl1' :: (Char -> Char -> Char) -> Text -> Char
foldl1' f t = S.foldl1' f (stream t)
{-# INLINE foldl1' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a 'Text',
-- reduces the 'Text' using the binary operator, from right to left.
-- Subject to array fusion.
foldr :: (Char -> b -> b) -> b -> Text -> b
foldr f z t = S.foldr f z (stream t)
{-# INLINE foldr #-}

-- | A variant of 'foldr' that has no starting value argument, and
-- thust must be applied to a non-empty 'Text'.  Subject to array
-- fusion.
foldr1 :: (Char -> Char -> Char) -> Text -> Char
foldr1 f t = S.foldr1 f (stream t)
{-# INLINE foldr1 #-}

-- -----------------------------------------------------------------------------
-- ** Special folds

-- | /O(n)/ Concatenate a list of 'Text's. Subject to array fusion.
concat :: [Text] -> Text
concat ts = unstream (S.concat (L.map stream ts))
{-# INLINE concat #-}

-- | Map a function over a 'Text' that results in a 'Text', and concatenate the
-- results.  This function is subject to array fusion.
--
-- Note: if in 'concatMap' @f @xs, @f@ is defined in terms of fusible
-- functions, it will also be fusible.
concatMap :: (Char -> Text) -> Text -> Text
concatMap f t = unstream (S.concatMap (stream . f) (stream t))
{-# INLINE concatMap #-}

-- | 'any' @p @xs determines whether any character in the 'Text' @xs@
-- satisifes the predicate @p@. Subject to array fusion.
any :: (Char -> Bool) -> Text -> Bool
any p t = S.any p (stream t)
{-# INLINE any #-}

-- | 'all' @p @xs determines whether all characters in the 'Text' @xs@
-- satisify the predicate @p@. Subject to array fusion.
all :: (Char -> Bool) -> Text -> Bool
all p t = S.all p (stream t)
{-# INLINE all #-}

-- | /O(n)/ 'maximum' returns the maximum value from a 'Text', which
-- must be non-empty. Subject to array fusion.
maximum :: Text -> Char
maximum t = S.maximum (stream t)
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'Text', which
-- must be non-empty. Subject to array fusion.
minimum :: Text -> Char
minimum t = S.minimum (stream t)
{-# INLINE minimum #-}

-- -----------------------------------------------------------------------------
-- * Building 'Text's

scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
scanl f z t = unstream (S.scanl f z (stream t))
{-# INLINE scanl #-}

-- -----------------------------------------------------------------------------
-- ** Generating and unfolding 'Text's

replicate :: Int -> Char -> Text
replicate n c = unstream (S.replicate n c)
{-# INLINE replicate #-}

-- | /O(n)/, where @n@ is the length of the result. The 'unfoldr'
-- function is analogous to the List 'L.unfoldr'. 'unfoldr' builds a
-- 'Text' from a seed value. The function takes the element and
-- returns 'Nothing' if it is done producing the 'Text', otherwise
-- 'Just' @(a,b)@.  In this case, @a@ is the next 'Char' in the
-- string, and @b@ is the seed value for further production.
unfoldr     :: (a -> Maybe (Char,a)) -> a -> Text
unfoldr f s = unstream (S.unfoldr f s)
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a 'Text' from a seed
-- value. However, the length of the result should be limited by the
-- first argument to 'unfoldrN'. This function is more efficient than
-- 'unfoldr' when the maximum length of the result is known and
-- correct, otherwise its performance is similar to 'unfoldr'.
unfoldrN     :: Int -> (a -> Maybe (Char,a)) -> a -> Text
unfoldrN n f s = unstream (S.unfoldrN n f s)
{-# INLINE unfoldrN #-}

-- -----------------------------------------------------------------------------
-- * Substrings

-- /O(n) 'take' @n@, applied to a 'Text', returns the prefix of the
-- 'Text' of length @n@, or the 'Text' itself if @n@ is greater than
-- the length of the Text.
take :: Int -> Text -> Text
take n t@(Text arr off len)
    | n <= 0    = empty
    | n >= len  = t
    | otherwise = Text arr off (loop off 0)
  where
      end = off+len
      loop !i !count
           | i >= end || count >= n   = i - off
           | c < 0xD800 || c > 0xDBFF = loop (i+1) (count+1)
           | otherwise                = loop (i+2) (count+1)
           where
             c = arr `A.unsafeIndex` i
{-# INLINE [1] take #-}

{-# RULES
"TEXT take -> fused" [~1] forall n t.
    take n t = unstream (S.take n (stream t))
"TEXT take -> unfused" [1] forall n t.
    unstream (S.take n (stream t)) = take n t
  #-}

-- | /O(n)/ 'drop' @n@, applied to a 'Text', returns the suffix of the
-- 'Text' of length @n@, or the empty 'Text' if @n@ is greater than the
-- length of the 'Text'.
drop :: Int -> Text -> Text
drop n t@(Text arr off len)
    | n <= 0    = t
    | n >= len  = empty
    | otherwise = Text arr newOff newLen
  where
      (newOff, newLen) = loop off 0 len
      end = off + len
      loop !i !count !l
          | i >= end || count >= n   = (i,l)
          | c < 0xD800 || c > 0xDBFF = loop (i+1) (count+1) (l-1)
          | otherwise                = loop (i+2) (count+1) (l-2)
          where
            c = arr `A.unsafeIndex` i
{-# INLINE [1] drop #-}

{-# RULES
"TEXT drop -> fused" [~1] forall n t.
    drop n t = unstream (S.drop n (stream t))
"TEXT drop -> unfused" [1] forall n t.
    unstream (S.drop n (stream t)) = drop n t
  #-}

-- | /O(n)/ 'takeWhile', applied to a predicate @p@ and a 'Text', returns the
-- longest prefix (possibly empty) of elements that satisfy @p@.
takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p t = unstream (S.takeWhile p (stream t))
{-# INLINE takeWhile #-}

-- | /O(n)/ 'dropWhile' @p@ @xs@ returns the suffix remaining after
-- 'takeWhile' @p@ @xs@.
dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p t = unstream (S.dropWhile p (stream t))
{-# INLINE dropWhile #-}

-- | /O(n)/ Return all initial segments of the given 'Text', shortest
-- first.
inits :: Text -> [Text]
inits t@(Text arr off len) = loop off
    where loop i | i >= len = [t]
                 | otherwise = Text arr off i : loop j
              where j | n < 0xD800 || n > 0xDBFF = i + 1
                      | otherwise                = i + 2
                    n = A.unsafeIndex arr i

-- | /O(n)/ Return all final segments of the given 'Text', longest
-- first.
tails :: Text -> [Text]
tails t | null t    = [empty]
        | otherwise = t : tails (tail t)

-- ----------------------------------------------------------------------------
-- * Searching

-------------------------------------------------------------------------------
-- ** Searching by equality

-- | /O(n)/ 'elem' is the 'Text' membership predicate.
elem :: Char -> Text -> Bool
elem c t = S.elem c (stream t)
{-# INLINE elem #-}

-------------------------------------------------------------------------------
-- ** Searching with a predicate

-- | /O(n)/ The 'find' function takes a predicate and a 'Text',
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> Text -> Maybe Char
find p t = S.find p (stream t)
{-# INLINE find #-}

-- | /O(n)/ 'filter', applied to a predicate and a 'Text',
-- returns a 'Text' containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> Text -> Text
filter p t = unstream (S.filter p (stream t))
{-# INLINE filter #-}


-------------------------------------------------------------------------------
-- ** Indexing 'Text's

-- | /O(1)/ 'Text' index (subscript) operator, starting from 0.
index :: Text -> Int -> Char
index t n = S.index (stream t) n
{-# INLINE index #-}

-- | The 'findIndex' function takes a predicate and a 'Text' and
-- returns the index of the first element in the 'Text'
-- satisfying the predicate.
findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex p t = S.findIndex p (stream t)
{-# INLINE findIndex #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'Text' which is equal to the query element, or
-- 'Nothing' if there is no such element.
elemIndex :: Char -> Text -> Maybe Int
elemIndex c t = S.elemIndex c (stream t)

-------------------------------------------------------------------------------
-- * Zipping

-- | /O(n)/ 'zipWith' generalises 'zip' by zipping with the function
-- given as the first argument, instead of a tupling function.
zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f t1 t2 = unstream (S.zipWith f (stream t1) (stream t2))

-- | /O(n)/ Breaks a 'Text' up into a list of words, delimited by 'Char's
-- representing white space.
words :: Text -> [Text]
words (Text arr off len) = loop0 off off
    where
      loop0 start n
            | isSpace (unsafeChr c) = if start == n
                                      then loop0 (start+1) (start+1)
                                      else (Text arr start (n-start)):loop0 (n+1) (n+1)
            | n < (off+len) = loop0 start (n+1)
            | otherwise = if start == n
                          then []
                          else [(Text arr start (n-start))]
            where
              c = arr `A.unsafeIndex` n
{-# INLINE words #-}

-- | /O(n)/ Breaks a 'Text' up into a list of 'Text's at
-- newline 'Char's. The resulting strings do not contain newlines.
--
lines :: Text -> [Text]
lines ps
    | null ps = []
    | otherwise = case search ps of
             Nothing -> [ps]
             Just n  -> take n ps : lines (drop (n+1) ps)
    where search = elemIndex '\n'
{-# INLINE lines #-}

-- | /O(n)/ Joins lines, after appending a terminating newline to
-- each.
unlines :: [Text] -> Text
unlines = concat . L.map (`snoc` '\n')
{-# INLINE unlines #-}

-- | /O(n)/ Joins words using single space characters.
unwords :: [Text] -> Text
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

errorEmptyList :: String -> a
errorEmptyList fun = error ("Data.Text." ++ fun ++ ": empty list")
