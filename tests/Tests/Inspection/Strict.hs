{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -fno-warn-unused-top-binds -fno-warn-missing-signatures -fno-warn-name-shadowing -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

module Tests.Inspection.Strict (tests) where

import Prelude hiding (all, any, drop, dropWhile, filter, foldl, foldl1, foldr, foldr1, head, init, iterate, last, length, map, maximum, minimum, null, reverse, scanl, scanl1, scanr, scanr1, tail, take, takeWhile)
import Data.Char (isAscii)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Test.Tasty
import Test.Tasty.Inspection

all_cons_pack = all . cons . T.pack
all_drop_pack = all . drop . T.pack
all_dropWhileEnd_singleton = all . dropWhileEnd . T.singleton
all_justifyRight_singleton = all . justifyRight . T.singleton
all_scanl_init_unfoldrN = all . scanl . T.init . unfoldrN
all_stripEnd_stripStart_singleton = all . T.stripEnd . T.stripStart . T.singleton
any_filter_init_pack = any . filter . T.init . T.pack
any_justifyRight_dropEnd_singleton = any . justifyRight . dropEnd . T.singleton
any_singleton = any . T.singleton
any_stripStart_singleton = any . T.stripStart . T.singleton
any_toCaseFold_unfoldr = any . T.toCaseFold . unfoldr
any_unfoldrN = any . unfoldrN
compareLength_cons_snoc_singleton = compareLength . cons . snoc . T.singleton
compareLength_drop_unfoldr = compareLength . drop . unfoldr
compareLength_empty = compareLength . empty
compareLength_pack = compareLength . T.pack
compareLength_singleton = compareLength . T.singleton
compareLength_snoc_unfoldr = compareLength . snoc . unfoldr
compareLength_takeWhile_intersperse_unfoldr = compareLength . takeWhile . intersperse . unfoldr
compareLength_toTitle_singleton = compareLength . T.toTitle . T.singleton
compareLength_toTitle_unfoldrN = compareLength . T.toTitle . unfoldrN
compareLength_unfoldr = compareLength . unfoldr
find_decodeUtf8 = find . T.decodeUtf8
find_take_unfoldrN = find . take . unfoldrN
foldl'_cons_pack = foldl' . cons . T.pack
foldl'_scanr1_singleton = foldl' . scanr1 . T.singleton
foldl'_toCaseFold_intersperse_unfoldrN = foldl' . T.toCaseFold . intersperse . unfoldrN
foldl_center_empty = foldl . center . empty
foldl_justifyLeft_cons_empty = foldl . justifyLeft . cons . empty
foldl_justifyLeft_decodeUtf8 = foldl . justifyLeft . T.decodeUtf8
foldl_pack = foldl . T.pack
foldl_scanl_dropWhile_empty = foldl . scanl . dropWhile . empty
foldl1'_append_append_decodeUtf8 = foldl1' . append . append . T.decodeUtf8
foldl1'_dropWhile_dropWhileEnd_singleton = foldl1' . dropWhile . dropWhileEnd . T.singleton
foldl1'_scanl_decodeUtf8 = foldl1' . scanl . T.decodeUtf8
foldl1'_scanl_justifyLeft_unfoldr = foldl1' . scanl . justifyLeft . unfoldr
foldl1'_singleton = foldl1' . T.singleton
foldl1'_take_unfoldr = foldl1' . take . unfoldr
foldl1_intersperse_empty = foldl1 . intersperse . empty
foldl1_scanr_singleton = foldl1 . scanr . T.singleton
foldl1_tail_singleton = foldl1 . T.tail . T.singleton
foldr_append_singleton = foldr . append . T.singleton
foldr_empty = foldr . empty
foldr_intersperse_dropWhile_unfoldr = foldr . intersperse . dropWhile . unfoldr
foldr_intersperse_singleton = foldr . intersperse . T.singleton
foldr_scanl_unfoldr = foldr . scanl . unfoldr
foldr1_dropWhile_intersperse_pack = foldr1 . dropWhile . intersperse . T.pack
foldr1_justifyLeft_scanl1_empty = foldr1 . justifyLeft . scanl1 . empty
foldr1_reverse_unfoldrN = foldr1 . T.reverse . unfoldrN
foldr1_singleton = foldr1 . T.singleton
foldr1_take_drop_pack = foldr1 . take . drop . T.pack
foldr1_unfoldrN = foldr1 . unfoldrN
head_append_toTitle_singleton = T.head . append . T.toTitle . T.singleton
head_cons_unfoldr = T.head . cons . unfoldr
head_drop_decodeUtf8 = T.head . drop . T.decodeUtf8
head_singleton = T.head . T.singleton
head_strip_take_empty = T.head . T.strip . take . empty
head_takeEnd_take_singleton = T.head . takeEnd . take . T.singleton
index_dropWhile_unfoldr = index . dropWhile . unfoldr
index_dropWhileEnd_empty = index . dropWhileEnd . empty
index_justifyLeft_stripEnd_singleton = index . justifyLeft . T.stripEnd . T.singleton
isPrefixOf_dropWhile_dropWhile_pack = isPrefixOf . dropWhile . dropWhile . T.pack
isPrefixOf_init_take_unfoldrN = isPrefixOf . T.init . take . unfoldrN
isPrefixOf_snoc_stripStart_pack = isPrefixOf . snoc . T.stripStart . T.pack
isPrefixOf_take_empty = isPrefixOf . take . empty
isPrefixOf_take_singleton = isPrefixOf . take . T.singleton
last_dropWhile_unfoldrN = T.last . dropWhile . unfoldrN
last_map_take_pack = T.last . map . take . T.pack
last_tail_unfoldrN = T.last . T.tail . unfoldrN
last_toUpper_stripStart_singleton = T.last . T.toUpper . T.stripStart . T.singleton
last_toUpper_toLower_unfoldrN = T.last . T.toUpper . T.toLower . unfoldrN
length_empty = T.length . empty
length_intersperse_center_singleton = T.length . intersperse . center . T.singleton
length_justifyLeft_decodeUtf8 = T.length . justifyLeft . T.decodeUtf8
length_pack = T.length . T.pack
length_reverse_singleton = T.length . T.reverse . T.singleton
length_takeWhile_intersperse_singleton = T.length . takeWhile . intersperse . T.singleton
length_takeWhile_singleton = T.length . takeWhile . T.singleton
length_toTitle_empty = T.length . T.toTitle . empty
maximum_justifyLeft_filter_singleton = T.maximum . justifyLeft . filter . T.singleton
maximum_justifyRight_singleton = T.maximum . justifyRight . T.singleton
maximum_take_unfoldrN = T.maximum . take . unfoldrN
maximum_toLower_empty = T.maximum . T.toLower . empty
minimum_init_singleton = T.minimum . T.init . T.singleton
minimum_intersperse_toTitle_singleton = T.minimum . intersperse . T.toTitle . T.singleton
minimum_map_singleton = T.minimum . map . T.singleton
minimum_scanl1_takeWhile_singleton = T.minimum . scanl1 . takeWhile . T.singleton
minimum_tail_map_singleton = T.minimum . T.tail . map . T.singleton
minimum_unfoldrN = T.minimum . unfoldrN
null_cons_singleton = T.null . cons . T.singleton
null_init_drop_decodeUtf8 = T.null . T.init . drop . T.decodeUtf8
null_map_empty = T.null . map . empty
null_toCaseFold_dropAround_singleton = T.null . T.toCaseFold . dropAround . T.singleton
unpack_empty = T.unpack . empty
unpack_justifyLeft_take_empty = T.unpack . justifyLeft . take . empty
unpack_map_pack = T.unpack . map . T.pack
unpack_stripEnd_takeWhileEnd_singleton = T.unpack . T.stripEnd . takeWhileEnd . T.singleton
unpack_toCaseFold_scanr_singleton = T.unpack . T.toCaseFold . scanr . T.singleton
unpack_toUpper_snoc_singleton = T.unpack . T.toUpper . snoc . T.singleton
unpack_unfoldr = T.unpack . unfoldr

tests :: TestTree
tests = testGroup "Strict fusion" [$(inspectNames (`hasNoType` ''T.Text)
  [
#if __GLASGOW_HASKELL__ >= 806
  'all_cons_pack, 'all_drop_pack, 'all_scanl_init_unfoldrN, 'any_filter_init_pack, 'any_toCaseFold_unfoldr, 'any_unfoldrN, 'compareLength_drop_unfoldr, 'compareLength_empty, 'compareLength_pack, 'compareLength_snoc_unfoldr, 'compareLength_takeWhile_intersperse_unfoldr, 'compareLength_toTitle_unfoldrN, 'compareLength_unfoldr, 'find_decodeUtf8, 'find_take_unfoldrN, 'foldl'_cons_pack, 'foldl'_toCaseFold_intersperse_unfoldrN, 'foldl_justifyLeft_cons_empty, 'foldl_justifyLeft_decodeUtf8, 'foldl_pack, 'foldl_scanl_dropWhile_empty, 'foldl1'_append_append_decodeUtf8, 'foldl1'_scanl_decodeUtf8, 'foldl1'_scanl_justifyLeft_unfoldr, 'foldl1'_take_unfoldr, 'foldl1_intersperse_empty, 'foldr_empty, 'foldr_intersperse_dropWhile_unfoldr, 'foldr_scanl_unfoldr, 'foldr1_dropWhile_intersperse_pack, 'foldr1_justifyLeft_scanl1_empty, 'foldr1_reverse_unfoldrN, 'foldr1_take_drop_pack, 'foldr1_unfoldrN, 'head_cons_unfoldr, 'head_drop_decodeUtf8, 'head_strip_take_empty, 'index_dropWhile_unfoldr, 'index_dropWhileEnd_empty, 'isPrefixOf_dropWhile_dropWhile_pack, 'isPrefixOf_init_take_unfoldrN, 'isPrefixOf_snoc_stripStart_pack, 'isPrefixOf_take_empty, 'last_dropWhile_unfoldrN, 'last_map_take_pack, 'last_tail_unfoldrN, 'last_toUpper_toLower_unfoldrN, 'length_empty, 'length_justifyLeft_decodeUtf8, 'length_pack, 'length_toTitle_empty, 'maximum_take_unfoldrN, 'maximum_toLower_empty, 'minimum_unfoldrN, 'null_init_drop_decodeUtf8, 'null_map_empty, 'unpack_empty, 'unpack_justifyLeft_take_empty, 'unpack_map_pack, 'unpack_unfoldr
#endif
-- https://gitlab.haskell.org/ghc/ghc/-/issues/19822
#if __GLASGOW_HASKELL__ >= 806 && __GLASGOW_HASKELL__ < 900
  , 'foldl_center_empty -- the only non-singleton-related regression
  , 'all_dropWhileEnd_singleton, 'all_justifyRight_singleton, 'all_stripEnd_stripStart_singleton, 'any_justifyRight_dropEnd_singleton, 'any_singleton, 'any_stripStart_singleton, 'compareLength_cons_snoc_singleton, 'compareLength_singleton, 'compareLength_toTitle_singleton, 'foldl'_scanr1_singleton, 'foldl1'_dropWhile_dropWhileEnd_singleton, 'foldl1_scanr_singleton, 'foldl1_tail_singleton, 'foldr_append_singleton, 'foldl1'_singleton, 'foldr_intersperse_singleton, 'foldr1_singleton, 'head_append_toTitle_singleton, 'head_singleton, 'head_takeEnd_take_singleton, 'isPrefixOf_take_singleton, 'index_justifyLeft_stripEnd_singleton, 'last_toUpper_stripStart_singleton, 'length_intersperse_center_singleton, 'length_reverse_singleton, 'length_takeWhile_intersperse_singleton, 'length_takeWhile_singleton, 'maximum_justifyLeft_filter_singleton, 'maximum_justifyRight_singleton, 'minimum_init_singleton, 'minimum_intersperse_toTitle_singleton, 'minimum_map_singleton, 'minimum_scanl1_takeWhile_singleton, 'minimum_tail_map_singleton, 'null_cons_singleton, 'null_toCaseFold_dropAround_singleton, 'unpack_stripEnd_takeWhileEnd_singleton, 'unpack_toCaseFold_scanr_singleton, 'unpack_toUpper_snoc_singleton
#endif
  ])]

---------------------------------------------------------------------------------
-- Definitions below are from inspection-testing package by Joachim Breitner.
--

i = 42
{-# NOINLINE i #-}

empty         _ = T.empty
{-# INLINE empty #-}
take          x = T.take i x
{-# INLINE take #-}
takeEnd       x = T.takeEnd i x
{-# INLINE takeEnd #-}
drop          x = T.drop i x
{-# INLINE drop #-}
dropEnd       x = T.dropEnd i x
{-# INLINE dropEnd #-}
cons          x = 'x' `T.cons` x
{-# INLINE cons #-}
snoc          x = x `T.snoc` 'x'
{-# INLINE snoc #-}
map           x = T.map succ x
{-# INLINE map #-}
justifyLeft   x = T.justifyLeft 42 'x' x
{-# INLINE justifyLeft #-}
justifyRight  x = T.justifyRight 42 'x' x
{-# INLINE justifyRight #-}
center        x = T.center i 'x' x
{-# INLINE center #-}
intersperse   x = T.intersperse 'x' x
{-# INLINE intersperse #-}
append        x = unfoldrN 'y' `T.append` x
{-# INLINE append #-}
isPrefixOf    x = unfoldrN 'a' `T.isPrefixOf` x
{-# INLINE isPrefixOf #-}
compareLength x = x `T.compareLength` i
{-# INLINE compareLength #-}
foldl         x = T.foldl   (\x c -> x + fromEnum c) 0 x
{-# INLINE foldl #-}
foldl'        x = T.foldl'  (\x c -> x + fromEnum c) 0 x
{-# INLINE foldl' #-}
foldl1        x = T.foldl1  (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2) x
{-# INLINE foldl1 #-}
foldl1'       x = T.foldl1' (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2) x
{-# INLINE foldl1' #-}
foldr         x = T.foldr   (\c x -> x + fromEnum c) 0 x
{-# INLINE foldr #-}
foldr1        x = T.foldr1  (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2) x
{-# INLINE foldr1 #-}
any           x = T.any isAscii x
{-# INLINE any #-}
all           x = T.all isAscii x
{-# INLINE all #-}
scanl         x = T.scanl  (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2) 'x' x
{-# INLINE scanl #-}
scanl1        x = T.scanl1 (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2)     x
{-# INLINE scanl1 #-}
scanr         x = T.scanr  (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2) 'x' x
{-# INLINE scanr #-}
scanr1        x = T.scanr1 (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2)     x
{-# INLINE scanr1 #-}
unfoldr       x = T.unfoldr    (\c -> if c  == 'z' then Nothing else Just (c, succ c)) x
{-# INLINE unfoldr #-}
unfoldrN      x = T.unfoldrN i (\c -> if c  == 'z' then Nothing else Just (c, succ c)) x
{-# INLINE unfoldrN #-}
takeWhile     x = T.takeWhile isAscii x
{-# INLINE takeWhile #-}
dropWhile     x = T.dropWhile isAscii x
{-# INLINE dropWhile #-}
takeWhileEnd  x = T.takeWhileEnd isAscii x
{-# INLINE takeWhileEnd #-}
dropWhileEnd  x = T.dropWhileEnd isAscii x
{-# INLINE dropWhileEnd #-}
dropAround    x = T.dropAround isAscii x
{-# INLINE dropAround #-}
filter        x = T.filter isAscii x
{-# INLINE filter #-}
find          x = T.find isAscii x
{-# INLINE find #-}
index         x = x `T.index` i
{-# INLINE index #-}
