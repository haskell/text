-- | Tests for operations that don't fit in the other @Test.Properties.*@ modules.

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules -fno-warn-missing-signatures #-}
module Tests.Properties.Text
    ( testText
    ) where

import Data.Char (isLower, isLetter, isUpper)
import Data.Maybe (mapMaybe)
import Data.Text.Internal.Fusion.Size
import Data.Word (Word8)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Tests.QuickCheckUtils
import Text.Show.Functions ()
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion as S
import qualified Data.Text.Internal.Fusion.Common as S
import qualified Data.Text.Internal.Lazy.Fusion as SL
import qualified Data.Text.Internal.Lazy.Search as S (indices)
import qualified Data.Text.Internal.Search as T (indices)
import qualified Data.Text.Lazy as TL
import qualified Tests.SlowFunctions as Slow

t_pack_unpack       = (T.unpack . T.pack) `eq` id
tl_pack_unpack      = (TL.unpack . TL.pack) `eq` id
t_stream_unstream   = (S.unstream . S.stream) `eq` id
tl_stream_unstream  = (SL.unstream . SL.stream) `eq` id
t_reverse_stream t  = (S.reverse . S.reverseStream) t === t
t_singleton c       = [c] === (T.unpack . T.singleton) c
tl_singleton c      = [c] === (TL.unpack . TL.singleton) c
tl_unstreamChunks x = f 11 x === f 1000 x
    where f n = SL.unstreamChunks n . S.streamList
tl_chunk_unchunk    = (TL.fromChunks . TL.toChunks) `eq` id
tl_from_to_strict   = (TL.fromStrict . TL.toStrict) `eq` id


s_map f           = map f  `eqP` (unpackS . S.map f)
s_map_s f         = map f  `eqP` (unpackS . S.unstream . S.map f)
sf_map p f        = (map f . L.filter p)  `eqP` (unpackS . S.map f . S.filter p)
t_map f           = map f  `eqP` (unpackS . T.map f)
tl_map f          = map f  `eqP` (unpackS . TL.map f)
s_intercalate c   = unsquare $
                    L.intercalate c `eq`
                    (unpackS . S.intercalate (packS c) . map packS)
t_intercalate c   = unsquare $
                    L.intercalate c `eq`
                    (unpackS . T.intercalate (packS c) . map packS)
tl_intercalate c  = unsquare $
                    L.intercalate c `eq`
                    (unpackS . TL.intercalate (TL.pack c) . map TL.pack)
s_intersperse c   = L.intersperse c `eqP`
                    (unpackS . S.intersperse c)
s_intersperse_s c = L.intersperse c `eqP`
                    (unpackS . S.unstream . S.intersperse c)
sf_intersperse p c= (L.intersperse c . L.filter p) `eqP`
                   (unpackS . S.intersperse c . S.filter p)
t_intersperse c   = unsquare $
                    L.intersperse c `eqP` (unpackS . T.intersperse c)
tl_intersperse c  = unsquare $
                    L.intersperse c `eqP` (unpackS . TL.intersperse c)
t_transpose       = unsquare $
                    L.transpose `eq` (map unpackS . T.transpose . map packS)
tl_transpose      = unsquare $
                    L.transpose `eq` (map unpackS . TL.transpose . map TL.pack)
t_reverse         = L.reverse `eqP` (unpackS . T.reverse)
tl_reverse        = L.reverse `eqP` (unpackS . TL.reverse)
t_reverse_short n = L.reverse `eqP` (unpackS . S.reverse . shorten n . S.stream)

t_replace s d     = (L.intercalate d . splitOn s) `eqP`
                    (unpackS . T.replace (T.pack s) (T.pack d))
tl_replace s d     = (L.intercalate d . splitOn s) `eqP`
                     (unpackS . TL.replace (TL.pack s) (TL.pack d))

splitOn :: (Eq a) => [a] -> [a] -> [[a]]
splitOn pat src0
    | l == 0    = error "splitOn: empty"
    | otherwise = go src0
  where
    l           = length pat
    go src      = search 0 src
      where
        search _ [] = [src]
        search !n s@(_:s')
            | pat `L.isPrefixOf` s = take n src : go (drop l s)
            | otherwise            = search (n+1) s'

s_toCaseFold_length xs = S.length (S.toCaseFold s) >= length xs
    where s = S.streamList xs
sf_toCaseFold_length p xs =
    (S.length . S.toCaseFold . S.filter p $ s) >= (length . L.filter p $ xs)
    where s = S.streamList xs
t_toCaseFold_length t = T.length (T.toCaseFold t) >= T.length t
tl_toCaseFold_length t = TL.length (TL.toCaseFold t) >= TL.length t
t_toLower_length t = T.length (T.toLower t) >= T.length t
t_toLower_lower t = p (T.toLower t) >= p t
    where p = T.length . T.filter isLower
tl_toLower_lower t = p (TL.toLower t) >= p t
    where p = TL.length . TL.filter isLower
t_toUpper_length t = T.length (T.toUpper t) >= T.length t
t_toUpper_upper t = p (T.toUpper t) >= p t
    where p = T.length . T.filter isUpper
tl_toUpper_upper t = p (TL.toUpper t) >= p t
    where p = TL.length . TL.filter isUpper
t_toTitle_title t = all (<= 1) (caps w)
    where caps = fmap (T.length . T.filter isUpper) . T.words . T.toTitle
          -- TIL: there exist uppercase-only letters
          w = T.filter (\c -> if C.isUpper c then C.toLower c /= c else True) t
t_toTitle_1stNotLower = and . notLow . T.toTitle . T.filter stable . T.filter (not . isGeorgian)
    where notLow = mapMaybe (fmap (not . isLower) . (T.find isLetter)) . T.words
          -- Surprise! The Spanish/Portuguese ordinal indicators changed
          -- from category Ll (letter, lowercase) to Lo (letter, other)
          -- in Unicode 7.0
          -- Oh, and there exist lowercase-only letters (see previous test)
          stable c = if isLower c
                     then C.toUpper c /= c
                     else c /= '\170' && c /= '\186'
          -- Georgian text does not have a concept of title case
          -- https://en.wikipedia.org/wiki/Georgian_Extended
          isGeorgian c = c >= '\4256' && c < '\4352'

justifyLeft k c xs  = xs ++ L.replicate (k - length xs) c
justifyRight m n xs = L.replicate (m - length xs) n ++ xs
center k c xs
    | len >= k  = xs
    | otherwise = L.replicate l c ++ xs ++ L.replicate r c
   where len = length xs
         d   = k - len
         r   = d `div` 2
         l   = d - r

s_justifyLeft k c = justifyLeft j c `eqP` (unpackS . S.justifyLeftI j c)
    where j = fromIntegral (k :: Word8)
s_justifyLeft_s k c = justifyLeft j c `eqP`
                      (unpackS . S.unstream . S.justifyLeftI j c)
    where j = fromIntegral (k :: Word8)
sf_justifyLeft p k c = (justifyLeft j c . L.filter p) `eqP`
                       (unpackS . S.justifyLeftI j c . S.filter p)
    where j = fromIntegral (k :: Word8)
t_justifyLeft k c = justifyLeft j c `eqP` (unpackS . T.justifyLeft j c)
    where j = fromIntegral (k :: Word8)
tl_justifyLeft k c = justifyLeft j c `eqP`
                     (unpackS . TL.justifyLeft (fromIntegral j) c)
    where j = fromIntegral (k :: Word8)
t_justifyRight k c = justifyRight j c `eqP` (unpackS . T.justifyRight j c)
    where j = fromIntegral (k :: Word8)
tl_justifyRight k c = justifyRight j c `eqP`
                      (unpackS . TL.justifyRight (fromIntegral j) c)
    where j = fromIntegral (k :: Word8)
t_center k c = center j c `eqP` (unpackS . T.center j c)
    where j = fromIntegral (k :: Word8)
tl_center k c = center j c `eqP` (unpackS . TL.center (fromIntegral j) c)
    where j = fromIntegral (k :: Word8)

t_elem c          = L.elem c `eqP` T.elem c
tl_elem c         = L.elem c `eqP` TL.elem c
sf_elem p c       = (L.elem c . L.filter p) `eqP` (S.elem c . S.filter p)
sf_filter q p     = (L.filter p . L.filter q) `eqP`
                    (unpackS . S.filter p . S.filter q)
t_filter p        = L.filter p    `eqP` (unpackS . T.filter p)
tl_filter p       = L.filter p    `eqP` (unpackS . TL.filter p)
sf_findBy q p     = (L.find p . L.filter q) `eqP` (S.findBy p . S.filter q)
t_find p          = L.find p      `eqP` T.find p
tl_find p         = L.find p      `eqP` TL.find p
t_partition p     = L.partition p `eqP` (unpack2 . T.partition p)
tl_partition p    = L.partition p `eqP` (unpack2 . TL.partition p)

sf_index p s      = forAll (choose (-l,l*2))
                    ((L.filter p s L.!!) `eq` S.index (S.filter p $ packS s))
    where l = L.length s
t_index s         = forAll (choose (-l,l*2)) ((s L.!!) `eq` T.index (packS s))
    where l = L.length s

tl_index s        = forAll (choose (-l,l*2))
                    ((s L.!!) `eq` (TL.index (packS s) . fromIntegral))
    where l = L.length s

t_findIndex p     = L.findIndex p `eqP` T.findIndex p
t_count (NotEmpty t)  = (subtract 1 . L.length . T.splitOn t) `eq` T.count t
tl_count (NotEmpty t) = (subtract 1 . L.genericLength . TL.splitOn t) `eq`
                        TL.count t
t_zip s           = L.zip s `eqP` T.zip (packS s)
tl_zip s          = L.zip s `eqP` TL.zip (packS s)
sf_zipWith p c s  = (L.zipWith c (L.filter p s) . L.filter p) `eqP`
                    (unpackS . S.zipWith c (S.filter p $ packS s) . S.filter p)
t_zipWith c s     = L.zipWith c s `eqP` (unpackS . T.zipWith c (packS s))
tl_zipWith c s    = L.zipWith c s `eqP` (unpackS . TL.zipWith c (packS s))

t_indices  (NotEmpty s) = Slow.indices s `eq` T.indices s
tl_indices (NotEmpty s) = lazyIndices s `eq` S.indices s
    where lazyIndices ss t = map fromIntegral $ Slow.indices (conc ss) (conc t)
          conc = T.concat . TL.toChunks
t_indices_occurs = unsquare $ \(NotEmpty t) ts ->
    let s = T.intercalate t ts
    in Slow.indices t s === T.indices t s

-- Make a stream appear shorter than it really is, to ensure that
-- functions that consume inaccurately sized streams behave
-- themselves.
shorten :: Int -> S.Stream a -> S.Stream a
shorten n t@(S.Stream arr off len)
    | n > 0     = S.Stream arr off (smaller (exactSize n) len)
    | otherwise = t

testText :: TestTree
testText =
  testGroup "Text" [
    testGroup "creation/elimination" [
      testProperty "t_pack_unpack" t_pack_unpack,
      testProperty "tl_pack_unpack" tl_pack_unpack,
      testProperty "t_stream_unstream" t_stream_unstream,
      testProperty "tl_stream_unstream" tl_stream_unstream,
      testProperty "t_reverse_stream" t_reverse_stream,
      testProperty "t_singleton" t_singleton,
      testProperty "tl_singleton" tl_singleton,
      testProperty "tl_unstreamChunks" tl_unstreamChunks,
      testProperty "tl_chunk_unchunk" tl_chunk_unchunk,
      testProperty "tl_from_to_strict" tl_from_to_strict
    ],

    testGroup "transformations" [
      testProperty "s_map" s_map,
      testProperty "s_map_s" s_map_s,
      testProperty "sf_map" sf_map,
      testProperty "t_map" t_map,
      testProperty "tl_map" tl_map,
      testProperty "s_intercalate" s_intercalate,
      testProperty "t_intercalate" t_intercalate,
      testProperty "tl_intercalate" tl_intercalate,
      testProperty "s_intersperse" s_intersperse,
      testProperty "s_intersperse_s" s_intersperse_s,
      testProperty "sf_intersperse" sf_intersperse,
      testProperty "t_intersperse" t_intersperse,
      testProperty "tl_intersperse" tl_intersperse,
      testProperty "t_transpose" t_transpose,
      testProperty "tl_transpose" tl_transpose,
      testProperty "t_reverse" t_reverse,
      testProperty "tl_reverse" tl_reverse,
      testProperty "t_reverse_short" t_reverse_short,
      testProperty "t_replace" t_replace,
      testProperty "tl_replace" tl_replace,

      testGroup "case conversion" [
        testProperty "s_toCaseFold_length" s_toCaseFold_length,
        testProperty "sf_toCaseFold_length" sf_toCaseFold_length,
        testProperty "t_toCaseFold_length" t_toCaseFold_length,
        testProperty "tl_toCaseFold_length" tl_toCaseFold_length,
        testProperty "t_toLower_length" t_toLower_length,
        testProperty "t_toLower_lower" t_toLower_lower,
        testProperty "tl_toLower_lower" tl_toLower_lower,
        testProperty "t_toUpper_length" t_toUpper_length,
        testProperty "t_toUpper_upper" t_toUpper_upper,
        testProperty "tl_toUpper_upper" tl_toUpper_upper,
        testProperty "t_toTitle_title" t_toTitle_title,
        testProperty "t_toTitle_1stNotLower" t_toTitle_1stNotLower
      ],

      testGroup "justification" [
        testProperty "s_justifyLeft" s_justifyLeft,
        testProperty "s_justifyLeft_s" s_justifyLeft_s,
        testProperty "sf_justifyLeft" sf_justifyLeft,
        testProperty "t_justifyLeft" t_justifyLeft,
        testProperty "tl_justifyLeft" tl_justifyLeft,
        testProperty "t_justifyRight" t_justifyRight,
        testProperty "tl_justifyRight" tl_justifyRight,
        testProperty "t_center" t_center,
        testProperty "tl_center" tl_center
      ]
    ],

    testGroup "searching" [
      testProperty "t_elem" t_elem,
      testProperty "tl_elem" tl_elem,
      testProperty "sf_elem" sf_elem,
      testProperty "sf_filter" sf_filter,
      testProperty "t_filter" t_filter,
      testProperty "tl_filter" tl_filter,
      testProperty "sf_findBy" sf_findBy,
      testProperty "t_find" t_find,
      testProperty "tl_find" tl_find,
      testProperty "t_partition" t_partition,
      testProperty "tl_partition" tl_partition
    ],

    testGroup "indexing" [
      testProperty "sf_index" sf_index,
      testProperty "t_index" t_index,
      testProperty "tl_index" tl_index,
      testProperty "t_findIndex" t_findIndex,
      testProperty "t_count" t_count,
      testProperty "tl_count" tl_count,
      testProperty "t_indices" t_indices,
      testProperty "tl_indices" tl_indices,
      testProperty "t_indices_occurs" t_indices_occurs
    ],

    testGroup "zips" [
      testProperty "t_zip" t_zip,
      testProperty "tl_zip" tl_zip,
      testProperty "sf_zipWith" sf_zipWith,
      testProperty "t_zipWith" t_zipWith,
      testProperty "tl_zipWith" tl_zipWith
    ]
  ]
