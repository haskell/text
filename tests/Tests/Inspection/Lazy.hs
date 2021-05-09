{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -fno-warn-unused-top-binds -fno-warn-missing-signatures -fno-warn-name-shadowing -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

module Tests.Inspection.Lazy (tests) where

import Prelude hiding (all, any, drop, dropWhile, filter, foldl, foldl1, foldr, foldr1, head, init, iterate, last, length, map, maximum, minimum, null, replicate, reverse, scanl, scanl1, scanr, scanr1, tail, take, takeWhile)
import Data.Char (isAscii)

import qualified Data.Text.Lazy as T

import Test.Tasty
import Test.Tasty.Inspection

maximum_unfoldr = T.maximum . unfoldr
isPrefixOf_unfoldr = isPrefixOf . unfoldr
any_filter_init_pack = any . filter . T.init . T.pack
isPrefixOf_intersperse_unfoldr = isPrefixOf . intersperse . unfoldr
compareLength_take_pack = compareLength . take . T.pack
index_toCaseFold_singleton = index . T.toCaseFold . T.singleton
unpack_take_pack = T.unpack . take . T.pack
foldl_singleton = foldl . T.singleton
foldr_filter_singleton = foldr . filter . T.singleton
foldr1_stripStart_singleton = foldr1 . T.stripStart . T.singleton
foldl_drop_singleton = foldl . drop . T.singleton
all_toCaseFold_unfoldr = all . T.toCaseFold . unfoldr
foldl_pack = foldl . T.pack
last_toUpper_toLower_unfoldrN = T.last . T.toUpper . T.toLower . unfoldrN
null_map_unfoldr = T.null . map . unfoldr
any_toTitle_tail_unfoldrN = any . T.toTitle . T.tail . unfoldrN
head_filter_pack = T.head . filter . T.pack
foldr1_toTitle_stripStart_singleton = foldr1 . T.toTitle . T.stripStart . T.singleton
compareLength_snoc_unfoldr = compareLength . snoc . unfoldr
compareLength_intersperse_toUpper_unfoldr = compareLength . intersperse . T.toUpper . unfoldr
compareLength_takeWhile_intersperse_unfoldr = compareLength . takeWhile . intersperse . unfoldr
all_init_map_unfoldrN = all . T.init . map . unfoldrN
foldr_scanl_unfoldrN = foldr . scanl . unfoldrN
compareLength_pack = compareLength . T.pack
foldr_cons_singleton = foldr . cons . T.singleton
last_tail_unfoldrN = T.last . T.tail . unfoldrN
foldr1_take_unfoldr = foldr1 . take . unfoldr
null_unfoldrN = T.null . unfoldrN
foldr1_init_singleton = foldr1 . T.init . T.singleton
all_cons_dropWhile_singleton = all . cons . dropWhile . T.singleton
head_append_toTitle_singleton = T.head . append . T.toTitle . T.singleton
foldl1_tail_intersperse_unfoldr = foldl1 . T.tail . intersperse . unfoldr
foldr1_drop_singleton = foldr1 . drop . T.singleton
minimum_tail_pack = T.minimum . T.tail . T.pack
minimum_pack = T.minimum . T.pack
length_takeWhile_singleton = T.length . takeWhile . T.singleton
foldl_take_toCaseFold_unfoldr = foldl . take . T.toCaseFold . unfoldr
foldr_intersperse_singleton = foldr . intersperse . T.singleton
compareLength_cons_snoc_singleton = compareLength . cons . snoc . T.singleton
last_map_append_unfoldrN = T.last . map . append . unfoldrN
find_justifyLeft_takeWhile_pack = find . justifyLeft . takeWhile . T.pack
head_append_unfoldr = T.head . append . unfoldr
minimum_justifyLeft_drop_unfoldrN = T.minimum . justifyLeft . drop . unfoldrN
null_singleton = T.null . T.singleton
last_map_justifyLeft_pack = T.last . map . justifyLeft . T.pack
foldl1'_tail_unfoldrN = foldl1' . T.tail . unfoldrN
maximum_take_unfoldrN = T.maximum . take . unfoldrN
foldl'_unfoldrN = foldl' . unfoldrN
foldl1'_filter_intersperse_singleton = foldl1' . filter . intersperse . T.singleton
length_singleton = T.length . T.singleton
foldr1_pack = foldr1 . T.pack
foldl1'_singleton = foldl1' . T.singleton
foldr1_scanl_singleton = foldr1 . scanl . T.singleton
minimum_tail_singleton = T.minimum . T.tail . T.singleton
any_pack = any . T.pack
length_pack = T.length . T.pack
head_unfoldrN = T.head . unfoldrN
head_map_pack = T.head . map . T.pack
foldr_stripStart_toCaseFold_singleton = foldr . T.stripStart . T.toCaseFold . T.singleton
any_append_pack = any . append . T.pack
all_drop_toUpper_unfoldrN = all . drop . T.toUpper . unfoldrN
foldl1'_filter_filter_unfoldr = foldl1' . filter . filter . unfoldr
any_singleton = any . T.singleton
any_toTitle_scanl_unfoldr = any . T.toTitle . scanl . unfoldr
foldr1_cons_pack = foldr1 . cons . T.pack
foldl1'_toTitle_dropWhile_singleton = foldl1' . T.toTitle . dropWhile . T.singleton
length_justifyLeft_unfoldr = T.length . justifyLeft . unfoldr
foldl1'_justifyLeft_pack = foldl1' . justifyLeft . T.pack
foldr_map_toTitle_unfoldrN = foldr . map . T.toTitle . unfoldrN
head_singleton = T.head . T.singleton
foldl'_singleton = foldl' . T.singleton
foldr1_dropWhile_intersperse_pack = foldr1 . dropWhile . intersperse . T.pack
foldl1'_pack = foldl1' . T.pack
head_replicate_singleton = T.head . replicate . T.singleton
unpack_toUpper_snoc_singleton = T.unpack . T.toUpper . snoc . T.singleton
null_empty = T.null . empty
maximum_singleton = T.maximum . T.singleton
isPrefixOf_init_singleton = isPrefixOf . T.init . T.singleton
minimum_unfoldr = T.minimum . unfoldr
foldl_stripStart_snoc_singleton = foldl . T.stripStart . snoc . T.singleton
any_toUpper_unfoldr = any . T.toUpper . unfoldr
all_unfoldr = all . unfoldr
minimum_toLower_unfoldrN = T.minimum . T.toLower . unfoldrN
null_pack = T.null . T.pack
index_dropWhile_unfoldr = index . dropWhile . unfoldr
minimum_filter_toUpper_singleton = T.minimum . filter . T.toUpper . T.singleton
head_pack = T.head . T.pack
foldl1'_toTitle_singleton = foldl1' . T.toTitle . T.singleton
find_unfoldr = find . unfoldr
isPrefixOf_unfoldrN = isPrefixOf . unfoldrN
unpack_append_pack = T.unpack . append . T.pack
any_unfoldr = any . unfoldr
length_unfoldrN = T.length . unfoldrN
minimum_singleton = T.minimum . T.singleton
head_snoc_toUpper_singleton = T.head . snoc . T.toUpper . T.singleton
maximum_unfoldrN = T.maximum . unfoldrN
all_take_pack = all . take . T.pack
isPrefixOf_pack = isPrefixOf . T.pack
foldr_init_pack = foldr . T.init . T.pack
foldl1'_filter_pack = foldl1' . filter . T.pack

tests :: TestTree
tests = testGroup "Lazy fusion" [$(inspectNames (`hasNoType` ''T.Text)
  ['maximum_unfoldr, 'isPrefixOf_unfoldr, 'any_filter_init_pack, 'isPrefixOf_intersperse_unfoldr, 'compareLength_take_pack, 'index_toCaseFold_singleton, 'unpack_take_pack, 'foldl_singleton, 'foldr_filter_singleton, 'foldr1_stripStart_singleton, 'foldl_drop_singleton, 'all_toCaseFold_unfoldr, 'foldl_pack, 'last_toUpper_toLower_unfoldrN, 'null_map_unfoldr, 'any_toTitle_tail_unfoldrN, 'head_filter_pack, 'foldr1_toTitle_stripStart_singleton, 'compareLength_snoc_unfoldr, 'compareLength_intersperse_toUpper_unfoldr, 'compareLength_takeWhile_intersperse_unfoldr, 'all_init_map_unfoldrN, 'foldr_scanl_unfoldrN, 'compareLength_pack, 'foldr_cons_singleton, 'last_tail_unfoldrN, 'foldr1_take_unfoldr, 'null_unfoldrN, 'foldr1_init_singleton, 'all_cons_dropWhile_singleton, 'head_append_toTitle_singleton, 'foldl1_tail_intersperse_unfoldr, 'foldr1_drop_singleton, 'minimum_tail_pack, 'minimum_pack, 'length_takeWhile_singleton, 'foldl_take_toCaseFold_unfoldr, 'foldr_intersperse_singleton, 'compareLength_cons_snoc_singleton, 'last_map_append_unfoldrN, 'find_justifyLeft_takeWhile_pack, 'head_append_unfoldr, 'minimum_justifyLeft_drop_unfoldrN, 'null_singleton, 'last_map_justifyLeft_pack, 'foldl1'_tail_unfoldrN, 'maximum_take_unfoldrN, 'foldl'_unfoldrN, 'foldl1'_filter_intersperse_singleton, 'length_singleton, 'foldr1_pack, 'foldl1'_singleton, 'foldr1_scanl_singleton, 'minimum_tail_singleton, 'any_pack, 'length_pack, 'head_unfoldrN, 'head_map_pack, 'foldr_stripStart_toCaseFold_singleton, 'any_append_pack, 'all_drop_toUpper_unfoldrN, 'foldl1'_filter_filter_unfoldr, 'any_singleton, 'any_toTitle_scanl_unfoldr, 'foldr1_cons_pack, 'foldl1'_toTitle_dropWhile_singleton, 'length_justifyLeft_unfoldr, 'foldl1'_justifyLeft_pack, 'foldr_map_toTitle_unfoldrN, 'head_singleton, 'foldl'_singleton, 'foldr1_dropWhile_intersperse_pack, 'foldl1'_pack, 'head_replicate_singleton, 'unpack_toUpper_snoc_singleton, 'null_empty, 'maximum_singleton, 'isPrefixOf_init_singleton, 'minimum_unfoldr, 'foldl_stripStart_snoc_singleton, 'any_toUpper_unfoldr, 'all_unfoldr, 'minimum_toLower_unfoldrN, 'null_pack, 'index_dropWhile_unfoldr, 'minimum_filter_toUpper_singleton, 'head_pack, 'foldl1'_toTitle_singleton, 'find_unfoldr, 'isPrefixOf_unfoldrN, 'unpack_append_pack, 'any_unfoldr, 'length_unfoldrN, 'minimum_singleton, 'head_snoc_toUpper_singleton, 'maximum_unfoldrN, 'all_take_pack, 'isPrefixOf_pack, 'foldr_init_pack, 'foldl1'_filter_pack])]

---------------------------------------------------------------------------------
-- Definitions below are from inspection-testing package by Joachim Breitner.
--

i = 42
{-# NOINLINE i #-}

empty         _ = T.empty
{-# INLINE empty #-}
take          x = T.take i x
{-# INLINE take #-}
drop          x = T.drop i x
{-# INLINE drop #-}
cons          x = 'x' `T.cons` x
{-# INLINE cons #-}
snoc          x = x `T.snoc` 'x'
{-# INLINE snoc #-}
map           x = T.map succ x
{-# INLINE map #-}
justifyLeft   x = T.justifyLeft 42 'x' x
{-# INLINE justifyLeft #-}
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
unfoldr       x = T.unfoldr    (\c -> if c  == 'z' then Nothing else Just (c, succ c)) x
{-# INLINE unfoldr #-}
unfoldrN      x = T.unfoldrN i (\c -> if c  == 'z' then Nothing else Just (c, succ c)) x
{-# INLINE unfoldrN #-}
takeWhile     x = T.takeWhile isAscii x
{-# INLINE takeWhile #-}
dropWhile     x = T.dropWhile isAscii x
{-# INLINE dropWhile #-}
filter        x = T.filter isAscii x
{-# INLINE filter #-}
find          x = T.find isAscii x
{-# INLINE find #-}
replicate     x = T.replicate i x
{-# INLINE replicate #-}
index         x = x `T.index` i
{-# INLINE index #-}
