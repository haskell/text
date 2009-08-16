{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.Text.Lazy
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtharper@aftereternity.co.uk,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- A time and space-efficient implementation of Unicode text using
-- lists of packed arrays.  This representation is suitable for high
-- performance use and for streaming large quantities of data.  It
-- provides a means to manipulate a large body of text without
-- requiring that the entire content be resident in memory.
--
-- Some operations, such as 'concat', 'append', 'reverse' and 'cons',
-- have better complexity than their "Data.Text" equivalents, due to
-- optimisations resulting from the list spine structure. And for
-- other operations lazy 'Text's are usually within a few percent of
-- strict ones, but with better heap usage. For data larger than
-- available memory, or if you have tight memory constraints, this
-- module will be the only option.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.Text.Lazy as B

module Data.Text.Lazy
    (
      Text
    -- * Creation and elimination
    , pack
    , unpack
    , singleton
    , empty
    , fromChunks
    , toChunks

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
    , replace

    -- ** Case conversion
    -- $case
    , toCaseFold
    , toLower
    , toUpper

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
    , drop
    , takeWhile
    , dropWhile
    , dropWhileEnd
    , dropAround
    , strip
    , stripStart
    , stripEnd
    , splitAt
    , span
    , break
    , group
    , groupBy
    , inits
    , tails

    -- ** Breaking into many substrings
    -- $split
    , split
    , splitTimes
    , splitTimesEnd
    , splitWith
    , chunksOf
    -- , breakSubstring

    -- ** Breaking into lines and words
    , lines
    , words
    , unlines
    , unwords

    -- * Predicates
    , isPrefixOf
    , isSuffixOf
    , isInfixOf

    -- * Searching
    , elem
    , filter
    , find
    , partition

    -- , findSubstring
    
    -- * Indexing
    , index
    , findIndex
    , findIndices
    , elemIndex
    , elemIndices
    , count

    -- * Zipping and unzipping
    , zip
    , zipWith

    -- -* Ordered text
    -- , sort
    ) where

import Prelude (Char, Bool(..), Int, Maybe(..), String,
                Eq(..), Ord(..), Read(..), Show(..),
                (&&), (||), (+), (-), (.), ($), (++),
                div, flip, fromIntegral, not, otherwise)
import qualified Prelude as P
import Data.Int (Int64)
import qualified Data.List as L
import Data.Char (isSpace)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Fusion.Common as S
import qualified Data.Text.Unsafe as T
import qualified Data.Text.Lazy.Fusion as S
import Data.Text.Lazy.Fusion (stream, unstream)
import Data.Text.Lazy.Internal

instance Eq Text where
    t1 == t2 = stream t1 == stream t2
    {-# INLINE (==) #-}

instance Ord Text where
    compare t1 t2 = compare (stream t1) (stream t2)
    {-# INLINE compare #-}

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

-- | /O(n)/ Convert a 'String' into a 'Text'.
--
-- This function is subject to array fusion.
pack :: String -> Text
pack s = unstream (S.streamList s)
{-# INLINE [1] pack #-}

-- | /O(n)/ Convert a 'Text' into a 'String'.
-- Subject to array fusion.
unpack :: Text -> String
unpack t = S.unstreamList (stream t)
{-# INLINE [1] unpack #-}

singleton :: Char -> Text
singleton c = Chunk (T.singleton c) Empty
{-# INLINE [1] singleton #-}

{-# RULES
"LAZY TEXT singleton -> fused" [~1] forall c.
    singleton c = unstream (S.singleton c)
"LAZY TEXT singleton -> unfused" [1] forall c.
    unstream (S.singleton c) = singleton c
  #-}

-- | /O(c)/ Convert a list of strict 'T.Text's into a lazy 'Text'.
fromChunks :: [T.Text] -> Text
fromChunks cs = L.foldr chunk Empty cs

-- | /O(n)/ Convert a lazy 'Text' into a list of strict 'T.Text's.
toChunks :: Text -> [T.Text]
toChunks cs = foldrChunks (:) [] cs

cons :: Char -> Text -> Text
cons c t = Chunk (T.singleton c) t
{-# INLINE [1] cons #-}

{-# RULES
"LAZY TEXT cons -> fused" [~1] forall c t.
    cons c t = unstream (S.cons c (stream t))
"LAZY TEXT cons -> unfused" [1] forall c t.
    unstream (S.cons c (stream t)) = cons c t
 #-}

snoc :: Text -> Char -> Text
snoc t c = foldrChunks Chunk (singleton c) t
{-# INLINE [1] snoc #-}

{-# RULES
"LAZY TEXT snoc -> fused" [~1] forall t c.
    snoc t c = unstream (S.snoc (stream t) c)
"LAZY TEXT snoc -> unfused" [1] forall t c.
    unstream (S.snoc (stream t) c) = snoc t c
 #-}

-- | /O(n\/c)/ Appends one 'Text' to another.  Subject to array
-- fusion.
append :: Text -> Text -> Text
append xs ys = foldrChunks Chunk ys xs
{-# INLINE [1] append #-}

{-# RULES
"LAZY TEXT append -> fused" [~1] forall t1 t2.
    append t1 t2 = unstream (S.append (stream t1) (stream t2))
"LAZY TEXT append -> unfused" [1] forall t1 t2.
    unstream (S.append (stream t1) (stream t2)) = append t1 t2
 #-}

-- | /O(1)/ Returns the first character and rest of a 'Text', or
-- 'Nothing' if empty. Subject to array fusion.
uncons :: Text -> Maybe (Char, Text)
uncons Empty = Nothing
uncons (Chunk t ts) =
    Just (T.unsafeHead t,
          if T.length t == 1 then ts else Chunk (T.unsafeTail t) ts)
{-# INLINE uncons #-}

-- | /O(1)/ Returns the first character of a 'Text', which must be
-- non-empty.  Subject to array fusion.
head :: Text -> Char
head t = S.head (stream t)
{-# INLINE head #-}

-- | /O(1)/ Returns all characters after the head of a 'Text', which
-- must be non-empty.  Subject to array fusion.
tail :: Text -> Text
tail (Chunk t ts) = chunk (T.tail t) ts
tail Empty        = emptyError "tail"
{-# INLINE [1] tail #-}

{-# RULES
"LAZY TEXT tail -> fused" [~1] forall t.
    tail t = unstream (S.tail (stream t))
"LAZY TEXT tail -> unfused" [1] forall t.
    unstream (S.tail (stream t)) = tail t
 #-}

-- | /O(1)/ Returns all but the last character of a 'Text', which must
-- be non-empty.  Subject to array fusion.
init :: Text -> Text
init (Chunk t0 ts0) = go t0 ts0
    where go t (Chunk t' ts) = Chunk t (go t' ts)
          go t Empty         = chunk (T.init t) Empty
init Empty = emptyError "init"
{-# INLINE [1] init #-}

{-# RULES
"LAZY TEXT init -> fused" [~1] forall t.
    init t = unstream (S.init (stream t))
"LAZY TEXT init -> unfused" [1] forall t.
    unstream (S.init (stream t)) = init t
 #-}

-- | /O(1)/ Tests whether a 'Text' is empty or not.  Subject to array
-- fusion.
null :: Text -> Bool
null Empty = True
null _     = False
{-# INLINE [1] null #-}

{-# RULES
"LAZY TEXT null -> fused" [~1] forall t.
    null t = S.null (stream t)
"LAZY TEXT null -> unfused" [1] forall t.
    S.null (stream t) = null t
 #-}

-- | /O(1)/ Returns the last character of a 'Text', which must be
-- non-empty.  Subject to array fusion.
last :: Text -> Char
last Empty        = emptyError "last"
last (Chunk t ts) = go t ts
    where go _ (Chunk t' ts') = go t' ts'
          go t' Empty         = T.last t'
{-# INLINE [1] last #-}

{-# RULES
"LAZY TEXT last -> fused" [~1] forall t.
    last t = S.last (stream t)
"LAZY TEXT last -> unfused" [1] forall t.
    S.last (stream t) = last t
  #-}

length :: Text -> Int64
length = foldlChunks go 0
    where go l t = l + fromIntegral (T.length t)
{-# INLINE [1] length #-}

{-# RULES
"LAZY TEXT length -> fused" [~1] forall t.
    length t = S.length (stream t)
"LAZY TEXT length -> unfused" [1] forall t.
    S.length (stream t) = length t
 #-}

-- | /O(n)/ 'map' @f@ @t@ is the 'Text' obtained by applying @f@ to
-- each element of @t@.  Subject to array fusion.
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

-- | /O(n)/ Left-justify a string to the given length, using the
-- specified fill character on the right. Subject to fusion. Examples:
--
-- > justifyLeft 7 'x' "foo"    == "fooxxxx"
-- > justifyLeft 3 'x' "foobar" == "foobar"
justifyLeft :: Int64 -> Char -> Text -> Text
justifyLeft k c t
    | len >= k  = t
    | otherwise = t `append` replicate (k-len) c
  where len = length t
{-# INLINE [1] justifyLeft #-}

{-# RULES
"TEXT justifyLeft -> fused" [~1] forall k c t.
    justifyLeft k c t = unstream (S.justifyLeftI k c (stream t))
"TEXT justifyLeft -> unfused" [1] forall k c t.
    unstream (S.justifyLeftI k c (stream t)) = justifyLeft k c t
  #-}

-- | /O(n)/ Right-justify a string to the given length, using the
-- specified fill character on the left. Examples:
--
-- > justifyRight 7 'x' "bar"    == "xxxxbar"
-- > justifyRight 3 'x' "foobar" == "foobar"
justifyRight :: Int64 -> Char -> Text -> Text
justifyRight k c t
    | len >= k  = t
    | otherwise = replicate (k-len) c `append` t
  where len = length t
{-# INLINE justifyRight #-}

-- | /O(n)/ Center a string to the given length, using the
-- specified fill character on either side. Examples:
--
-- > center 8 'x' "HS" = "xxxHSxxx"
center :: Int64 -> Char -> Text -> Text
center k c t
    | len >= k  = t
    | otherwise = replicate l c `append` t `append` replicate r c
  where len = length t
        d   = k - len
        r   = d `div` 2
        l   = d - r
{-# INLINE center #-}

-- | /O(n)/ The 'transpose' function transposes the rows and columns
-- of its 'Text' argument.  Note that this function uses 'pack',
-- 'unpack', and the list version of transpose, and is thus not very
-- efficient.
transpose :: [Text] -> [Text]
transpose ts = L.map (\ss -> Chunk (T.pack ss) Empty)
                     (L.transpose (L.map unpack ts))
-- TODO: make this fast

-- | /O(n)/ 'reverse' @t@ returns the elements of @t@ in reverse order.
reverse :: Text -> Text
reverse = rev Empty
  where rev a Empty        = a
        rev a (Chunk t ts) = rev (Chunk (T.reverse t) a) ts

-- | /O(m)*O(n)/ Replace every occurrence of one substring with another.
replace :: Text                 -- ^ Text to search for
        -> Text                 -- ^ Replacement text
        -> Text                 -- ^ Input text
        -> Text
replace s d = intercalate d . split s
{-# INLINE replace #-}

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

-- | /O(n)/ Convert a string to folded case.  This function is mainly
-- useful for performing caseless (or case insensitive) string
-- comparisons.
--
-- A string @x@ is a caseless match for a string @y@ if and only if:
--
-- @toCaseFold x == toCaseFold y@
--
-- The result string may be longer than the input string, and may
-- differ from applying 'toLower' to the input string.  For instance,
-- the Armenian small ligature men now (U+FB13) is case folded to the
-- bigram men now (U+0574 U+0576), while the micro sign (U+00B5) is
-- case folded to the Greek small letter letter mu (U+03BC) instead of
-- itself.
toCaseFold :: Text -> Text
toCaseFold t = unstream (S.toCaseFold (stream t))
{-# INLINE [0] toCaseFold #-}

-- | /O(n)/ Convert a string to lower case, using simple case
-- conversion.  The result string may be longer than the input string.
-- For instance, the Latin capital letter I with dot above (U+0130)
-- maps to the sequence Latin small letter i (U+0069) followed by
-- combining dot above (U+0307).
toLower :: Text -> Text
toLower t = unstream (S.toLower (stream t))
{-# INLINE toLower #-}

-- | /O(n)/ Convert a string to upper case, using simple case
-- conversion.  The result string may be longer than the input string.
-- For instance, the German eszett (U+00DF) maps to the two-letter
-- sequence SS.
toUpper :: Text -> Text
toUpper t = unstream (S.toUpper (stream t))
{-# INLINE toUpper #-}

-- | /O(n)/ 'foldl', applied to a binary operator, a starting value
-- (typically the left-identity of the operator), and a 'Text',
-- reduces the 'Text' using the binary operator, from left to right.
-- Subject to array fusion.
foldl :: (b -> Char -> b) -> b -> Text -> b
foldl f z t = S.foldl f z (stream t)
{-# INLINE foldl #-}

-- | /O(n)/ A strict version of 'foldl'.
-- Subject to array fusion.
foldl' :: (b -> Char -> b) -> b -> Text -> b
foldl' f z t = S.foldl' f z (stream t)
{-# INLINE foldl' #-}

-- | /O(n)/ A variant of 'foldl' that has no starting value argument,
-- and thus must be applied to a non-empty 'Text'.  Subject to array
-- fusion.
foldl1 :: (Char -> Char -> Char) -> Text -> Char
foldl1 f t = S.foldl1 f (stream t)
{-# INLINE foldl1 #-}

-- | /O(n)/ A strict version of 'foldl1'.
-- Subject to array fusion.
foldl1' :: (Char -> Char -> Char) -> Text -> Char
foldl1' f t = S.foldl1' f (stream t)
{-# INLINE foldl1' #-}

-- | /O(n)/ 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a 'Text',
-- reduces the 'Text' using the binary operator, from right to left.
-- Subject to array fusion.
foldr :: (Char -> b -> b) -> b -> Text -> b
foldr f z t = S.foldr f z (stream t)
{-# INLINE foldr #-}

-- | /O(n)/ A variant of 'foldr' that has no starting value argument, and
-- thust must be applied to a non-empty 'Text'.  Subject to array
-- fusion.
foldr1 :: (Char -> Char -> Char) -> Text -> Char
foldr1 f t = S.foldr1 f (stream t)
{-# INLINE foldr1 #-}

-- | /O(n)/ Concatenate a list of 'Text's. Subject to array fusion.
concat :: [Text] -> Text
concat ts = unstream (S.concat (L.map stream ts))
{-# INLINE concat #-}

-- | /O(n)/ Map a function over a 'Text' that results in a 'Text', and
-- concatenate the results.  This function is subject to array fusion.
--
-- Note: if in 'concatMap' @f@ @t@, @f@ is defined in terms of fusible
-- functions, it will also be fusible.
concatMap :: (Char -> Text) -> Text -> Text
concatMap f t = unstream (S.concatMap (stream . f) (stream t))
{-# INLINE concatMap #-}

-- | /O(n)/ 'any' @p@ @t@ determines whether any character in the
-- 'Text' @t@ satisifes the predicate @p@. Subject to array fusion.
any :: (Char -> Bool) -> Text -> Bool
any p t = S.any p (stream t)
{-# INLINE any #-}

-- | /O(n)/ 'all' @p@ @t@ determines whether all characters in the
-- 'Text' @t@ satisify the predicate @p@. Subject to array fusion.
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

-- | /O(n)/ 'scanl' is similar to 'foldl', but returns a list of
-- successive reduced values from the left. This function is subject
-- to array fusion.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
scanl f z t = unstream (S.scanl f z (stream t))
{-# INLINE scanl #-}

-- | /O(n)/ 'scanl1' is a variant of 'scanl' that has no starting
-- value argument.  This function is subject to array fusion.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Char -> Char -> Char) -> Text -> Text
scanl1 f t0 = case uncons t0 of
                Nothing -> empty
                Just (t,ts) -> scanl f t ts
{-# INLINE scanl1 #-}

-- | /O(n)/ 'scanr' is the right-to-left dual of 'scanl'.
--
-- > scanr f v == reverse . scanl (flip f) v . reverse
scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
scanr f v = reverse . scanl (flip f) v . reverse

-- | /O(n)/ 'scanr1' is a variant of 'scanr' that has no starting
-- value argument.
scanr1 :: (Char -> Char -> Char) -> Text -> Text
scanr1 f t | null t    = empty
           | otherwise = scanr f (last t) (init t)

-- | /O(n)/ Like a combination of 'map' and 'foldl'. Applies a
-- function to each element of a 'Text', passing an accumulating
-- parameter from left to right, and returns a final 'Text'.
mapAccumL :: (a -> Char -> (a,Char)) -> a -> Text -> (a, Text)
mapAccumL f s t = case uncons t of
                    Nothing -> (s, empty)
                    Just (x, xs) -> (s'', cons y ys)
                        where (s', y ) = f s x
                              (s'',ys) = mapAccumL f s' xs

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a 'Text', passing
-- an accumulating parameter from right to left, and returning a final
-- value of this accumulator together with the new 'Text'.
mapAccumR :: (a -> Char -> (a,Char)) -> a -> Text -> (a, Text)
mapAccumR f s t = case uncons t of
                    Nothing -> (s, empty)
                    Just (x, xs) ->  (s'', cons y ys)
                        where (s'',y ) = f s' x
                              (s', ys) = mapAccumR f s xs

-- | /O(n)/ 'replicate' @n@ @c@ is a 'Text' of length @n@ with @c@ the
-- value of every element.
replicate :: Int64 -> Char -> Text
replicate n c = unstream (S.replicateI n c)
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
unfoldrN     :: Int64 -> (a -> Maybe (Char,a)) -> a -> Text
unfoldrN n f s = unstream (S.unfoldrN n f s)
{-# INLINE unfoldrN #-}

-- | /O(n)/ 'take' @n@, applied to a 'Text', returns the prefix of the
-- 'Text' of length @n@, or the 'Text' itself if @n@ is greater than
-- the length of the Text. Subject to fusion.
take :: Int64 -> Text -> Text
take i _ | i <= 0 = Empty
take i t0         = take' i t0
  where take' 0 _            = Empty
        take' _ Empty        = Empty
        take' n (Chunk t ts)
            | n < len   = Chunk (T.take (fromIntegral n) t) Empty
            | otherwise = Chunk t (take' (n - len) ts)
            where len = fromIntegral (T.length t)
{-# INLINE [1] take #-}

{-# RULES
"LAZY TEXT take -> fused" [~1] forall n t.
    take n t = unstream (S.take n (stream t))
"LAZY TEXT take -> unfused" [1] forall n t.
    unstream (S.take n (stream t)) = take n t
  #-}

-- | /O(n)/ 'drop' @n@, applied to a 'Text', returns the suffix of the
-- 'Text' of length @n@, or the empty 'Text' if @n@ is greater than the
-- length of the 'Text'. Subject to fusion.
drop :: Int64 -> Text -> Text
drop i t0
    | i <= 0 = t0
    | otherwise = drop' i t0
  where drop' 0 ts           = ts
        drop' _ Empty        = Empty
        drop' n (Chunk t ts) 
            | n < len = Chunk (T.drop (fromIntegral n) t) ts
            | otherwise = drop' (n - len) ts
            where len = fromIntegral (T.length t)
{-# INLINE [1] drop #-}

{-# RULES
"LAZY TEXT drop -> fused" [~1] forall n t.
    drop n t = unstream (S.drop n (stream t))
"LAZY TEXT drop -> unfused" [1] forall n t.
    unstream (S.drop n (stream t)) = drop n t
  #-}

-- | /O(n)/ 'takeWhile', applied to a predicate @p@ and a 'Text', returns
-- the longest prefix (possibly empty) of elements that satisfy @p@.
-- This function is subject to array fusion.
takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p t0 = takeWhile' t0
  where takeWhile' Empty        = Empty
        takeWhile' (Chunk t ts) =
          case T.findIndex (not . p) t of
            Just n | n > 0     -> Chunk (T.take n t) Empty
                   | otherwise -> Empty
            Nothing            -> Chunk t (takeWhile' ts)
{-# INLINE [1] takeWhile #-}

{-# RULES
"LAZY TEXT takeWhile -> fused" [~1] forall p t.
    takeWhile p t = unstream (S.takeWhile p (stream t))
"LAZY TEXT takeWhile -> unfused" [1] forall p t.
    unstream (S.takeWhile p (stream t)) = takeWhile p t
  #-}

-- | /O(n)/ 'dropWhile' @p@ @t@ returns the suffix remaining after
-- 'takeWhile' @p@ @t@. This function is subject to array fusion.
dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p t0 = dropWhile' t0
  where dropWhile' Empty        = Empty
        dropWhile' (Chunk t ts) =
          case T.findIndex (not . p) t of
            Just n  -> Chunk (T.drop n t) ts
            Nothing -> dropWhile' ts
{-# INLINE [1] dropWhile #-}

{-# RULES
"LAZY TEXT dropWhile -> fused" [~1] forall p t.
    dropWhile p t = unstream (S.dropWhile p (stream t))
"LAZY TEXT dropWhile -> unfused" [1] forall p t.
    unstream (S.dropWhile p (stream t)) = dropWhile p t
  #-}
-- | /O(n)/ 'dropWhileEnd' @p@ @t@ returns the prefix remaining after
-- dropping characters that fail the predicate @p@ from the end of
-- @t@.
-- Examples:
--
-- > dropWhileEnd (=='.') "foo..." == "foo"
dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p = go
  where go Empty = Empty
        go (Chunk t Empty) = if T.null t'
                             then Empty
                             else Chunk t' Empty
            where t' = T.dropWhileEnd p t
        go (Chunk t ts) = case go ts of
                            Empty -> go (Chunk t Empty)
                            ts' -> Chunk t ts'
{-# INLINE dropWhileEnd #-}

-- | /O(n)/ 'dropAround' @p@ @t@ returns the substring remaining after
-- dropping characters that fail the predicate @p@ from both the
-- beginning and end of @t@.  Subject to fusion.
dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p
{-# INLINE [1] dropAround #-}

-- | /O(n)/ Remove leading white space from a string.  Equivalent to:
--
-- > dropWhile isSpace
stripStart :: Text -> Text
stripStart = dropWhile isSpace
{-# INLINE [1] stripStart #-}

-- | /O(n)/ Remove trailing white space from a string.  Equivalent to:
--
-- > dropWhileEnd isSpace
stripEnd :: Text -> Text
stripEnd = dropWhileEnd isSpace
{-# INLINE [1] stripEnd #-}

-- | /O(n)/ Remove leading and trailing white space from a string.
-- Equivalent to:
--
-- > dropAround isSpace
strip :: Text -> Text
strip = dropAround isSpace
{-# INLINE [1] strip #-}

-- | /O(n)/ 'splitAt' @n t@ returns a pair whose first element is a
-- prefix of @t@ of length @n@, and whose second is the remainder of
-- the string. It is equivalent to @('take' n t, 'drop' n t)@.
splitAt :: Int64 -> Text -> (Text, Text)
splitAt = loop
  where loop _ Empty      = (empty, empty)
        loop n t | n <= 0 = (empty, t)
        loop n (Chunk t ts)
             | n < len   = let (t',t'') = T.splitAt (fromIntegral n) t
                           in (Chunk t' Empty, Chunk t'' ts)
             | otherwise = let (ts',ts'') = loop (n - len) ts
                           in (Chunk t ts', ts'')
             where len = fromIntegral (T.length t)

-- | /O(n)/ 'break' is like 'span', but the prefix returned is over
-- elements that fail the predicate @p@.
break :: (Char -> Bool) -> Text -> (Text, Text)
break p t0 = break' t0
  where break' Empty          = (empty, empty)
        break' c@(Chunk t ts) =
          case T.findIndex p t of
            Nothing      -> let (ts', ts'') = break' ts
                            in (Chunk t ts', ts'')
            Just n | n == 0    -> (Empty, c)
                   | otherwise -> let (a,b) = T.splitAt n t
                                  in (Chunk a Empty, Chunk b ts)

-- | /O(n)/ 'span', applied to a predicate @p@ and text @t@, returns a
-- pair whose first element is the longest prefix (possibly empty) of
-- @t@ of elements that satisfy @p@, and whose second is the remainder
-- of the list.
span :: (Char -> Bool) -> Text -> (Text, Text)
span p = break (not . p)
{-# INLINE span #-}

-- | The 'group' function takes a 'Text' and returns a list of 'Text's
-- such that the concatenation of the result is equal to the argument.
-- Moreover, each sublist in the result contains only equal elements.
-- For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test.
group :: Text -> [Text]
group =  groupBy (==)
{-# INLINE group #-}

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy _  Empty        = []
groupBy eq (Chunk t ts) = cons x ys : groupBy eq zs
                          where (ys,zs) = span (eq x) xs
                                x  = T.unsafeHead t
                                xs = chunk (T.unsafeTail t) ts

-- | /O(n)/ Return all initial segments of the given 'Text',
-- shortest first.
inits :: Text -> [Text]
inits = (Empty :) . inits'
  where inits' Empty        = []
        inits' (Chunk t ts) = L.map (\t' -> Chunk t' Empty) (L.tail (T.inits t))
                           ++ L.map (Chunk t) (inits' ts)

-- | /O(n)/ Return all final segments of the given 'Text', longest
-- first.
tails :: Text -> [Text]
tails Empty         = Empty : []
tails ts@(Chunk t ts')
  | T.length t == 1 = ts : tails ts'
  | otherwise       = ts : tails (Chunk (T.unsafeTail t) ts')

-- $split
--
-- Splitting functions in this library do not perform character-wise
-- copies to create substrings; they just construct new 'Text's that
-- are slices of the original.

-- | /O(m)*O(n)/ Break a 'Text' into pieces separated by the first
-- 'Text' argument, consuming the delimiter. Examples:
--
-- > split "\r\n" "a\r\nb\r\nd\r\ne" == ["a","b","d","e"]
-- > split "aaa"  "aaaXaaaXaaaXaaa"  == ["","X","X","X",""]
-- > split "x"    "x"                == ["",""]
-- 
-- and
--
-- > intercalate s . split s         == id
-- > split (singleton c)             == splitWith (==c)
split :: Text                   -- ^ Text to split on
      -> Text                   -- ^ Input text
      -> [Text]
split pat src0
    | l == 0    = [src0]
    | l == 1    = splitWith (== (head pat)) src0
    | otherwise = go src0
  where
    l      = length pat
    go src = search 0 src
      where
        search !n !s
            | null s             = [src]      -- not found
            | pat `isPrefixOf` s = take n src : go (drop l s)
            | otherwise          = search (n+1) (tail s)
{-# INLINE [1] split #-}

{-# RULES
"LAZY TEXT split/singleton -> splitWith/==" [~1] forall c t.
    split (singleton c) t = splitWith (==c) t
  #-}

-- | /O(m)*O(n)/ Break a 'Text' into pieces at most @k@ times,
-- treating the first 'Text' argument as the delimiter to break on,
-- and consuming the delimiter.  The last element of the list contains
-- the remaining text after the number of times to split has been
-- reached.  A value of zero or less for @k@ causes no splitting to
-- occur.
--
-- Examples:
--
-- > splitTimes 0   "//"  "a//b//c"   == ["a//b//c"]
-- > splitTimes 2   ":"   "a:b:c:d:e" == ["a","b","c:d:e"]
-- > splitTimes 100 "???" "a????b"    == ["a","?b"]
--
-- and
--
-- > intercalate s . splitTimes k s   == id
splitTimes :: Int64             -- ^ Maximum number of times to split
           -> Text              -- ^ Text to split on
           -> Text              -- ^ Input text
           -> [Text]
splitTimes k pat src0
    | k <= 0 || l == 0 = [src0]
    | otherwise        = go k src0
  where
    l         = length pat
    go !i src = search 0 src
      where
        search !n !s
            | i == 0 || null s   = [src]      -- not found or limit reached
            | pat `isPrefixOf` s = take n src : go (i-1) (drop l s)
            | otherwise          = search (n+1) (tail s)
{-# INLINE splitTimes #-}

-- | /O(m)*O(n)/ Break a 'Text' into pieces at most @k@ times, like
-- 'splitTimes', but start from the end of the input and work towards
-- the start.
--
-- Examples:
--
-- > splitTimes 2    "::" "a::b::c::d::e" == ["a","b","c::d::e"]
-- > splitTimesEnd 2 "::" "a::b::c::d::e" == ["a::b::c","d","e"]
splitTimesEnd :: Int64             -- ^ Maximum number of times to split
              -> Text              -- ^ Text to split on
              -> Text              -- ^ Input text
              -> [Text]
splitTimesEnd k pat src =
    L.reverse . L.map reverse $ splitTimes k (reverse pat) (reverse src)
{-# INLINE splitTimesEnd #-}

-- | /O(n)/ Splits a 'Text' into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith (=='a') []        == [""]
splitWith :: (Char -> Bool) -> Text -> [Text]
splitWith _ Empty = [Empty]
splitWith p (Chunk t0 ts0) = comb [] (T.splitWith p t0) ts0
  where comb acc (s:[]) Empty        = revChunks (s:acc) : []
        comb acc (s:[]) (Chunk t ts) = comb (s:acc) (T.splitWith p t) ts
        comb acc (s:ss) ts           = revChunks (s:acc) : comb [] ss ts
        comb _   []     _            = impossibleError "splitWith"
{-# INLINE splitWith #-}

-- | /O(n)/ Splits a 'Text' into components of length @k@.  The last
-- element may be shorter than the other chunks, depending on the
-- length of the input. Examples:
--
-- > chunksOf 3 "foobarbaz"   == ["foo","bar","baz"]
-- > chunksOf 4 "haskell.org" == ["hask","ell.","org"]
chunksOf :: Int64 -> Text -> [Text]
chunksOf k = go
  where
    go t = case splitAt k t of
             (a,b) | null a    -> []
                   | otherwise -> a : go b
{-# INLINE chunksOf #-}

-- | /O(n)/ Breaks a 'Text' up into a list of 'Text's at
-- newline 'Char's. The resulting strings do not contain newlines.
lines :: Text -> [Text]
lines Empty = []
lines t = let (l,t') = break ((==) '\n') t
          in l : if null t' then []
                 else lines (tail t')

-- | /O(n)/ Breaks a 'Text' up into a list of words, delimited by 'Char's
-- representing white space.
words :: Text -> [Text]
words = L.filter (not . null) . splitWith isSpace
{-# INLINE words #-}

-- | /O(n)/ Joins lines, after appending a terminating newline to
-- each.
unlines :: [Text] -> Text
unlines = concat . L.map (`snoc` '\n')
{-# INLINE unlines #-}

-- | /O(n)/ Joins words using single space characters.
unwords :: [Text] -> Text
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

-- | /O(n)/ The 'isPrefixOf' function takes two 'Text's and returns
-- 'True' iff the first is a prefix of the second.  This function is
-- subject to fusion.
isPrefixOf :: Text -> Text -> Bool
isPrefixOf Empty _  = True
isPrefixOf _ Empty  = False
isPrefixOf (Chunk x xs) (Chunk y ys)
    | lx == ly  = x == y  && isPrefixOf xs ys
    | lx <  ly  = x == yh && isPrefixOf xs (Chunk yt ys)
    | otherwise = xh == y && isPrefixOf (Chunk xt xs) ys
  where (xh,xt) = T.splitAt ly x
        (yh,yt) = T.splitAt lx y
        lx = T.length x
        ly = T.length y
{-# INLINE [1] isPrefixOf #-}

{-# RULES
"LAZY TEXT isPrefixOf -> fused" [~1] forall s t.
    isPrefixOf s t = S.isPrefixOf (stream s) (stream t)
"LAZY TEXT isPrefixOf -> unfused" [1] forall s t.
    S.isPrefixOf (stream s) (stream t) = isPrefixOf s t
  #-}

-- | /O(n)/ The 'isSuffixOf' function takes two 'Text's and returns
-- 'True' iff the first is a suffix of the second.
isSuffixOf :: Text -> Text -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y
{-# INLINE isSuffixOf #-}
-- TODO: a better implementation

-- | /O(n)/ The 'isInfixOf' function takes two 'Text's and returns
-- 'True' iff the first is contained, wholly and intact, anywhere
-- within the second.
isInfixOf :: Text -> Text -> Bool
isInfixOf needle haystack = L.any (isPrefixOf needle) (tails haystack)
{-# INLINE isInfixOf #-}
-- TODO: a better implementation

-- | /O(n)/ 'elem' is the 'Text' membership predicate.
elem :: Char -> Text -> Bool
elem c t = S.elem c (stream t)
{-# INLINE elem #-}

-- | /O(n)/ 'filter', applied to a predicate and a 'Text',
-- returns a 'Text' containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> Text -> Text
filter p t = unstream (S.filter p (stream t))
{-# INLINE filter #-}

-- | /O(n)/ The 'find' function takes a predicate and a 'Text',
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> Text -> Maybe Char
find p t = S.find p (stream t)
{-# INLINE find #-}

-- | /O(n)/ The 'partition' function takes a predicate and a 'Text',
-- and returns the pair of 'Text's with elements which do and do not
-- satisfy the predicate, respectively; i.e.
--
-- > partition p t == (filter p t, filter (not . p) t)
partition :: (Char -> Bool) -> Text -> (Text, Text)
partition p t = (filter p t, filter (not . p) t)
{-# INLINE partition #-}

-- | /O(n)/ 'Text' index (subscript) operator, starting from 0.
index :: Text -> Int64 -> Char
index t n = S.index (stream t) n
{-# INLINE index #-}

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'Text'
-- and returns the index of the first element in the 'Text' satisfying
-- the predicate. This function is subject to fusion.
findIndex :: (Char -> Bool) -> Text -> Maybe Int64
findIndex p t = S.findIndex p (stream t)
{-# INLINE findIndex #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending
-- order. This function is subject to fusion.
findIndices :: (Char -> Bool) -> Text -> [Int64]
findIndices p t = S.findIndices p (stream t)
{-# INLINE findIndices #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'Text' which is equal to the query element, or
-- 'Nothing' if there is no such element. This function is subject to
-- fusion.
elemIndex :: Char -> Text -> Maybe Int64
elemIndex c t = S.elemIndex c (stream t)
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndices' function returns the index of every
-- element in the given 'Text' which is equal to the query
-- element. This function is subject to fusion.
elemIndices :: Char -> Text -> [Int64]
elemIndices c t = S.elemIndices c (stream t)
{-# INLINE elemIndices #-}

-- | /O(n)/ The 'count' function returns the number of times the query
-- element appears in the given 'Text'. This function is subject to
-- fusion.
count :: Char -> Text -> Int64
count c t = S.count c (stream t)
{-# INLINE count #-}

-- | /O(n)/ 'zip' takes two 'Text's and returns a list of
-- corresponding pairs of bytes. If one input 'Text' is short,
-- excess elements of the longer 'Text' are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: Text -> Text -> [(Char,Char)]
zip a b = S.unstreamList $ S.zipWith (,) (stream a) (stream b)
{-# INLINE [0] zip #-}

-- | /O(n)/ 'zipWith' generalises 'zip' by zipping with the function
-- given as the first argument, instead of a tupling function.
zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f t1 t2 = unstream (S.zipWith f (stream t1) (stream t2))
{-# INLINE [0] zipWith #-}

revChunks :: [T.Text] -> Text
revChunks = L.foldl' (flip chunk) Empty

emptyError :: String -> a
emptyError fun = P.error ("Data.Text.Lazy." ++ fun ++ ": empty input")

impossibleError :: String -> a
impossibleError fun = P.error ("Data.Text.Lazy." ++ fun ++ ": impossible case")
