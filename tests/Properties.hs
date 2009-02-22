{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

import Test.QuickCheck
import Text.Show.Functions

import Prelude 
import Debug.Trace
import Text.Printf
import System.Environment
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Word
import qualified Data.Text as T
import Data.Text (pack,unpack)
import qualified Data.Text.Fusion as S
import Data.Text.Fusion (stream, unstream)
import qualified Data.List as L


import QuickCheckUtils

prop_pack_unpack s   = (unpack . pack) s == s
prop_stream_unstream t = (unstream . stream) t == t
prop_reverse_stream t = (S.reverse . S.reverseStream) t == t
prop_singleton c     = [c] == (unpack . T.singleton) c

-- Do two functions give the same answer?
eq :: (Eq a) => (t -> a) -> (t -> a) -> t -> Bool
eq a b s  = a s == b s
-- What about with the RHS packed?
eqP :: (Eq a, Show a) => (String -> a) -> (T.Text -> a) -> String -> Word8 -> Bool
eqP a b s w  = eq "orig" (a s) (b t) &&
               eq "head" (a sa) (b ta) &&
               eq "tail" (a sb) (b tb)
    where t             = pack s
          (sa,sb)       = splitAt m s
          (ta,tb)       = T.splitAt m t
          l             = length s
          m | l == 0    = n
            | otherwise = n `mod` l
          n             = fromIntegral w
          eq s a b | a == b = True
                   | otherwise = trace (s ++ ": " ++ show a ++ " /= " ++ show b) False
-- Or with the string non-empty, and the RHS packed?
eqEP :: (Eq a) =>
        (String -> a) -> (T.Text -> a) -> NotEmpty String -> Word8 -> Bool
eqEP a b e w  = a s == b t &&
                (null sa || a sa == b ta) &&
                (null sb || a sb == b tb)
    where (sa,sb)       = splitAt m s
          (ta,tb)       = T.splitAt m t
          t             = pack s
          l             = length s
          m | l == 0    = n
            | otherwise = n `mod` l
          n             = fromIntegral w
          s             = notEmpty e

prop_cons x          = (x:)     `eqP` (unpack . T.cons x)
prop_snoc x          = (++ [x]) `eqP` (unpack . (flip T.snoc) x)
prop_append s        = (s++)    `eqP` (unpack . T.append (pack s))
prop_appendS s       = (s++)    `eqP` (unpack . unstream . S.append (stream (pack s)) . stream)
prop_uncons s        = uncons   `eqP` (fmap (second unpack) . T.uncons)
    where uncons (x:xs) = Just (x,xs)
          uncons _      = Nothing
          types         = s :: String
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
prop_reverse_short n = L.reverse `eqP` (unpack . S.reverse . shorten n . stream)

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
prop_maximum         = L.maximum     `eqEP` T.maximum
prop_minimum         = L.minimum     `eqEP` T.minimum

prop_scanl f z       = L.scanl f z   `eqP`  (unpack . T.scanl f z)
prop_scanl1 f        = L.scanl1 f    `eqP`  (unpack . T.scanl1 f)
prop_scanr f z       = L.scanr f z   `eqP`  (unpack . T.scanr f z)

prop_mapAccumL f z   = (snd . L.mapAccumL f z)`eqP` (unpack . T.mapAccumL f z)
    where types = f :: Int -> Char -> (Int,Char)

prop_replicate n     = L.replicate n `eq`   (unpack . T.replicate n)
prop_unfoldr n       = L.unfoldr f   `eq`   (unpack . T.unfoldr f)
    where f c | fromEnum c * 100 > n = Nothing
              | otherwise            = Just (c, succ c)

prop_unfoldrN n m    = (L.take n . L.unfoldr f) `eq` (unpack . T.unfoldrN n f)
    where f c | fromEnum c * 100 > m = Nothing
              | otherwise            = Just (c, succ c)

unpack2 = unpack *** unpack

prop_take n          = L.take n      `eqP` (unpack . T.take n)
prop_drop n          = L.drop n      `eqP` (unpack . T.drop n)
prop_takeWhile p     = L.takeWhile p `eqP` (unpack . T.takeWhile p)
prop_takeWhileS p    = L.takeWhile p `eqP` (unpack . unstream . S.takeWhile p . stream)
prop_dropWhile p     = L.dropWhile p `eqP` (unpack . T.dropWhile p)
prop_dropWhileS p    = L.dropWhile p `eqP` (unpack . unstream . S.dropWhile p . stream)
prop_splitAt n       = L.splitAt n   `eqP` (unpack2 . T.splitAt n)
prop_span p          = L.span p      `eqP` (unpack2 . T.span p)
prop_break p         = L.break p     `eqP` (unpack2 . T.break p)
prop_group           = L.group       `eqP` (map unpack . T.group)
prop_groupBy p       = L.groupBy p   `eqP` (map unpack . T.groupBy p)
prop_inits           = L.inits       `eqP` (map unpack . T.inits)
prop_tails           = L.tails       `eqP` (map unpack . T.tails)

prop_split_i c       = id `eq` (T.intercalate (T.singleton c) . T.split c)

{-
prop_splitWith p     = splitWith p `eqP` (map unpack . T.splitWith p)

splitWith _ [] = [""]
splitWith p xs = h : splitWith p (whale t)
    where (h,t) = break p xs
          whale (_:xs) = xs
          whale xs = xs
-}

prop_breakSubstring_isInfixOf s l
                     = T.isInfixOf s l ==
                       T.null s || (not . T.null . snd $ T.breakSubstring s l)
prop_breakSubstringC c
                     = L.break (==c) `eqP`
                       (unpack2 . T.breakSubstring (T.singleton c))

prop_lines           = L.lines       `eqP` (map unpack . T.lines)
prop_words           = L.words       `eqP` (map unpack . T.words)
prop_unlines         = L.unlines     `eq`  (unpack . T.unlines . map pack)
prop_unwords         = L.unwords     `eq`  (unpack . T.unwords . map pack)

prop_isPrefixOf s    = L.isPrefixOf s`eqP` T.isPrefixOf (pack s)
prop_isPrefixOfS s   = L.isPrefixOf s`eqP` (S.isPrefixOf (stream $ pack s) . stream)
prop_isSuffixOf s    = L.isSuffixOf s`eqP` T.isSuffixOf (pack s)
prop_isInfixOf s     = L.isInfixOf s `eqP` T.isInfixOf (pack s)

prop_elem c          = L.elem c      `eqP` T.elem c
prop_filter p        = L.filter p    `eqP` (unpack . T.filter p)
prop_find p          = L.find p      `eqP` T.find p
prop_partition p     = L.partition p `eqP` (unpack2 . T.partition p)

prop_index x s       = x < L.length s && x >= 0 ==>
                       (L.!!) s x == T.index (pack s) x
prop_findIndex p     = L.findIndex p `eqP` T.findIndex p
prop_findIndices p   = L.findIndices p`eqP` T.findIndices p
prop_elemIndex c     = L.elemIndex c `eqP` T.elemIndex c
prop_elemIndices c   = L.elemIndices c`eqP` T.elemIndices c
prop_count c         = (L.length . L.elemIndices c) `eqP` T.count c
prop_zipWith c s     = L.zipWith c s `eqP` (unpack . T.zipWith c (pack s))

-- Make a stream appear shorter than it really is, to ensure that
-- functions that consume inaccurately sized streams behave
-- themselves.
shorten :: Int -> S.Stream a -> S.Stream a
shorten n t@(S.Stream arr off len)
    | n < len && n > 0 = S.Stream arr off n
    | otherwise        = t

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
  ("prop_reverse_stream", mytest prop_reverse_stream),
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
  ("prop_reverse", mytest prop_reverse),
  ("prop_reverse_short", mytest prop_reverse_short),

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
  ("prop_maximum", mytest prop_maximum),
  ("prop_minimum", mytest prop_minimum),

  ("prop_scanl", mytest prop_scanl),
  ("prop_scanl1", mytest prop_scanl1),
  ("prop_scanr", mytest prop_scanr),

  ("prop_mapAccumL", mytest prop_mapAccumL),

  ("prop_replicate", mytest prop_replicate),
  ("prop_unfoldr", mytest prop_unfoldr),
  ("prop_unfoldrN", mytest prop_unfoldrN),

  ("prop_take", mytest prop_take),
  ("prop_drop", mytest prop_drop),
  ("prop_takeWhile", mytest prop_takeWhile),
  ("prop_takeWhileS", mytest prop_takeWhileS),
  ("prop_dropWhile", mytest prop_dropWhile),
  ("prop_dropWhileS", mytest prop_dropWhileS),
  ("prop_splitAt", mytest prop_splitAt),
  ("prop_span", mytest prop_span),
  ("prop_break", mytest prop_break),
  ("prop_group", mytest prop_group),
  ("prop_groupBy", mytest prop_groupBy),
  ("prop_inits", mytest prop_inits),
  ("prop_tails", mytest prop_tails),

  ("prop_split_i", mytest prop_split_i),
--("prop_splitWith", mytest prop_splitWith),
  ("prop_breakSubstringC", mytest prop_breakSubstringC),
  ("prop_breakSubstring_isInfixOf", mytest prop_breakSubstring_isInfixOf),

  ("prop_lines", mytest prop_lines),
  ("prop_words", mytest prop_words),
  ("prop_unlines", mytest prop_unlines),
  ("prop_unwords", mytest prop_unwords),

  ("prop_isPrefixOf", mytest prop_isPrefixOf),
  ("prop_isPrefixOfS", mytest prop_isPrefixOfS),
  ("prop_isSuffixOf", mytest prop_isSuffixOf),
  ("prop_isInfixOf", mytest prop_isInfixOf),

  ("prop_elem", mytest prop_elem),
  ("prop_filter", mytest prop_filter),
  ("prop_find", mytest prop_find),
  ("prop_partition", mytest prop_partition),

  ("prop_index", mytest prop_index),
  ("prop_findIndex", mytest prop_findIndex),
  ("prop_findIndices", mytest prop_findIndices),
  ("prop_elemIndex", mytest prop_elemIndex),
  ("prop_elemIndices", mytest prop_elemIndices),
  ("prop_count", mytest prop_count),
  ("prop_zipWith", mytest prop_zipWith)
  ]
