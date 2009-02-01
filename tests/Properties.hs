{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

import Test.QuickCheck
import Text.Show.Functions

import Prelude 
import Debug.Trace
import Text.Printf
import System.Environment
import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import Data.Text (pack,unpack)
import qualified Data.Text.Fusion as S
import Data.Text.Fusion (unstream,stream)
import qualified Data.List as L


import QuickCheckUtils

prop_pack_unpack s   = (unpack . pack) s == s
prop_stream_unstream t = (unstream . stream) t == t
prop_singleton c     = [c] == (unpack . T.singleton) c

-- Do two functions give the same answer?
eq a b s  = a s == b s
-- What about with the RHS packed?
eqP a b s  = a s == b (pack s)
-- Or with the string non-empty, and the RHS packed?
eqEP a b s = let e = notEmpty s
             in a e == b (pack e)

prop_cons x          = (x:)     `eqP` (unpack . T.cons x)
prop_snoc x          = (++ [x]) `eqP` (unpack . (flip T.snoc) x)
prop_append s        = (s++)    `eqP` (unpack . T.append (pack s))
prop_appendS s       = (s++)    `eqP` (unpack . unstream . S.append (stream (pack s)) . stream)
prop_uncons s        = case T.uncons (pack s) of
                         Nothing     -> null s
                         Just (x,xs) -> x == head s && unpack xs == tail s
prop_head            = head   `eqEP` T.head
prop_last            = last   `eqEP` T.last
prop_lastS           = last   `eqEP` (S.last . stream)
prop_tail            = tail   `eqEP` (unpack . T.tail)
prop_tailS           = tail   `eqEP` (unpack . unstream . S.tail . stream)
prop_init            = init   `eqEP` (unpack . T.init)
prop_initS           = init   `eqEP` (unpack . unstream . S.init . stream)
prop_null            = null   `eqP`  T.null
prop_length          = length `eqP`  T.length
prop_map f           = map f  `eqP`  (unpack . T.map f)
prop_intercalate c   = L.intercalate c `eq` (unpack . T.intercalate (pack c) . map pack)
prop_intersperse c   = L.intersperse c `eqP` (unpack . T.intersperse c)
prop_transpose       = L.transpose `eq` (map unpack . T.transpose . map pack)
prop_reverse         = L.reverse `eqP` (unpack . T.reverse)

prop_foldl f z       = L.foldl f z  `eqP`  (T.foldl f z)
    where types      = f :: Char -> Char -> Char
prop_foldl' f z      = L.foldl' f z `eqP`  T.foldl' f z
    where types      = f :: Char -> Char -> Char
prop_foldl1 f        = L.foldl1 f   `eqEP` T.foldl1 f
prop_foldl1' f       = L.foldl1' f  `eqEP` T.foldl1' f
prop_foldr f z       = L.foldr f z  `eqP`  T.foldr f z
    where types      = f :: Char -> Char -> Char
prop_foldr1 f        = L.foldr1 f   `eqEP` T.foldr1 f

prop_concat          = L.concat      `eq`   (unpack . T.concat . map pack)
prop_concatMap f     = L.concatMap f `eqP`  (unpack . T.concatMap (pack . f))
prop_any p           = L.any p       `eqP`  T.any p
prop_all p           = L.all p       `eqP`  T.all p
prop_minimum         = L.minimum     `eqEP` T.minimum
prop_maximum         = L.maximum     `eqEP` T.maximum

prop_take n          = L.take n      `eqP` (unpack . T.take n)
prop_drop n          = L.drop n      `eqP` (unpack . T.drop n)
prop_takeWhile p     = L.takeWhile p `eqP` (unpack . T.takeWhile p)
prop_dropWhile p     = L.dropWhile p `eqP` (unpack . T.dropWhile p)
prop_elem c          = L.elem c      `eqP` T.elem c
prop_find p          = L.find p      `eqP` T.find p
prop_filter p        = L.filter p    `eqP` (unpack . T.filter p)
prop_index x s       = x < L.length s && x >= 0 ==>
                       (L.!!) s x == T.index (pack s) x
prop_findIndex p     = L.findIndex p `eqP` T.findIndex p
prop_elemIndex c     = L.elemIndex c `eqP` T.elemIndex c
prop_zipWith c s     = L.zipWith c s `eqP` (unpack . T.zipWith c (pack s))
prop_words           = L.words       `eqP` (L.map unpack . T.words)

main = run tests =<< getArgs

run :: [(String, Int -> IO (Bool,Int))] -> [String] -> IO ()
run tests args = do
  let n = case args of
            [s] -> read s
            []  -> 100
            _   -> error "too many arguments"
  (results,passed) <- unzip <$> mapM (\(s,a) -> printf "%-40s: " s >> a n) tests
  printf "Passed %d tests!\n" (sum passed)
  when (not . and $ results) $
      fail "Not all tests passed!"

tests :: [(String, Int -> IO (Bool, Int))]
tests = [
  ("prop_pack_unpack", mytest prop_pack_unpack),
  ("prop_stream_unstream", mytest prop_stream_unstream),
  ("prop_singleton", mytest prop_singleton),

  ("prop_cons", mytest prop_cons),
  ("prop_snoc", mytest prop_snoc),
  ("prop_append", mytest prop_append),
  ("prop_appendS", mytest prop_appendS),
  ("prop_uncons", mytest prop_uncons),
  ("prop_head", mytest prop_head),
  ("prop_last", mytest prop_last),
  ("prop_lastS", mytest prop_lastS),
  ("prop_tail", mytest prop_tail),
  ("prop_tailS", mytest prop_tailS),
  ("prop_init", mytest prop_init),
  ("prop_initS", mytest prop_initS),
  ("prop_null", mytest prop_null),
  ("prop_length", mytest prop_length),

  ("prop_map", mytest prop_map),
  ("prop_intercalate", mytest prop_intercalate),
  ("prop_intersperse", mytest prop_intersperse),
  ("prop_transpose", mytest prop_transpose),
--("prop_reverse", mytest prop_reverse),

  ("prop_foldl", mytest prop_foldl),
  ("prop_foldl'", mytest prop_foldl'),
  ("prop_foldl1", mytest prop_foldl1),
  ("prop_foldl1'", mytest prop_foldl1'),
  ("prop_foldr", mytest prop_foldr),
  ("prop_foldr1", mytest prop_foldr1),

  ("prop_concat", mytest prop_concat),
  ("prop_concatMap", mytest prop_concatMap),
  ("prop_any", mytest prop_any),
  ("prop_all", mytest prop_all),
  ("prop_minimum", mytest prop_minimum),
  ("prop_maximum", mytest prop_maximum),

  ("prop_take", mytest prop_take),
  ("prop_drop", mytest prop_drop),
  ("prop_takeWhile", mytest prop_takeWhile),
  ("prop_dropWhile", mytest prop_dropWhile),
  ("prop_elem", mytest prop_elem),
  ("prop_find", mytest prop_find),
  ("prop_filter", mytest prop_filter),
  ("prop_index", mytest prop_index),
  ("prop_findIndex", mytest prop_findIndex),
  ("prop_elemIndex", mytest prop_elemIndex),
  ("prop_zipWith", mytest prop_zipWith),
  ("prop_words", mytest prop_words)
  ]
