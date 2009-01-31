{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

import Test.QuickCheck
import Text.Show.Functions

import Prelude 
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

prop_cons x xs       = (x:xs) == (unpack . T.cons x . pack) xs
prop_snoc x xs       = (xs ++ [x]) == (unpack . (flip T.snoc) x . pack) xs
prop_append s1 s2    = (s1 ++ s2) == (unpack $ T.append (pack s1) (pack s2))
prop_appendS s1 s2   = (s1 ++ s2) == ((unpack . unstream) $ S.append ((stream . pack) s1) ((stream . pack) s2))
prop_head s          = not (null s) ==> head s == (T.head . pack) s
prop_last s          = not (null s) ==> last s == (T.last . pack) s
prop_lastS s         = not (null s) ==> last s == (S.last . stream . pack) s
prop_tail s          = not (null s) ==> tail s == (unpack . T.tail . pack) s
prop_tailS s         = not (null s) ==> tail s == (unpack . unstream . S.tail . stream . pack) s
prop_init s          = not (null s) ==> init s == (unpack . T.init . pack) s
prop_initS s         = not (null s) ==> init s == (unpack . unstream . S.init . stream . pack) s
prop_null s          = null s == (T.null . pack) s
prop_length s        = length s == (T.length . pack) s
prop_map f s         = (map f s) == (unpack . T.map f . pack) s
prop_intersperse c s = (L.intersperse c s) == (unpack . T.intersperse c . pack) s
prop_transpose ss    = (L.transpose ss) == (map unpack . T.transpose . map pack) ss

prop_foldl f z s     = L.foldl f z s == T.foldl f z (pack s)
    where types = f :: Char -> Char -> Char
prop_foldl' f z s    = L.foldl' f z s == T.foldl' f z (pack s)
prop_foldl1 f s      = not (null s) ==> L.foldl1 f s == T.foldl1 f (pack s)
prop_foldl1' f s     = not (null s) ==> L.foldl1' f s == T.foldl1' f (pack s)
prop_foldr f z s     = L.foldr f z s == T.foldr f z (pack s)
    where types = f :: Char -> Char -> Char
prop_foldr1 f s      = not (null s) ==> L.foldr1 f s == T.foldr1 f (pack s)

prop_concat ss       = (L.concat ss) == (unpack . T.concat . map pack) ss    
prop_concatMap f s   = (L.concatMap f s) == (unpack (T.concatMap (pack . f) (pack s)))
prop_any p s         = L.any p s == T.any p (pack s)
prop_all p s         = L.all p s == T.all p (pack s)
prop_minimum s       = not (null s) ==> L.minimum s == T.minimum (pack s)
prop_maximum s       = not (null s) ==> L.maximum s == T.maximum (pack s)

prop_take n s        = L.take n s == (unpack . T.take n . pack) s
prop_drop n s        = L.drop n s == (unpack . T.drop n . pack) s
prop_takeWhile p s   = L.takeWhile p s == (unpack . T.takeWhile p . pack) s
prop_dropWhile p s   = L.dropWhile p s == (unpack . T.dropWhile p . pack) s
prop_elem c s        = L.elem c s == (T.elem c . pack) s
prop_find p s        = L.find p s == (T.find p . pack) s
prop_filter p s      = L.filter p s == (unpack . T.filter p . pack) s
prop_index x s       = x < L.length s && x >= 0 ==> (L.!!) s x == T.index (pack s) x
prop_findIndex p s   = L.findIndex p s == T.findIndex p (pack s)
prop_elemIndex c s   = L.elemIndex c s == T.elemIndex c (pack s)
prop_zipWith c s1 s2 = L.zipWith c s1 s2 == unpack (T.zipWith c (pack s1) (pack s2))
prop_words s         = L.words s == L.map unpack (T.words (pack s))

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
  ("prop_intersperse", mytest prop_intersperse),
  ("prop_transpose", mytest prop_transpose),

  ("prop_foldl", mytest prop_foldl),
  ("prop_foldl", mytest prop_foldl),
  ("prop_foldl1", mytest prop_foldl1),
  ("prop_foldl1", mytest prop_foldl1),
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
