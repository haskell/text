{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

import Test.QuickCheck
import Text.Show.Functions

import Data.Char
import Debug.Trace
import Text.Printf
import System.Environment
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as E
import Control.Exception
import qualified Data.Text.Fusion as S
import qualified Data.Text.Lazy.Fusion as SL
import qualified Data.List as L
import System.IO.Unsafe
import Prelude hiding (catch)

import QuickCheckUtils

-- If a pure property threatens to crash, wrap it with this to keep
-- QuickCheck from bombing out.
crashy :: a -> a -> a
{-# NOINLINE crashy #-}
crashy onException p = unsafePerformIO $
    (return $! p) `catch` \e ->
    let types = e :: SomeException
    in trace ("*** Exception: " ++ show e) return onException

prop_T_pack_unpack       = (T.unpack . T.pack) `eq` id
prop_TL_pack_unpack      = (TL.unpack . TL.pack) `eq` id
prop_T_stream_unstream   = (S.unstream . S.stream) `eq` id
prop_TL_stream_unstream  = (SL.unstream . SL.stream) `eq` id
prop_T_reverse_stream t  = (S.reverse . S.reverseStream) t == t
prop_T_singleton c       = [c] == (T.unpack . T.singleton) c

prop_T_ascii t           = E.decodeASCII (E.encodeUtf8 a) == a
    where a            = T.map (\c -> chr (ord c `mod` 128)) t
prop_T_utf8              = (E.decodeUtf8 . E.encodeUtf8) `eq` id
prop_T_utf16LE           = (E.decodeUtf16LE . E.encodeUtf16LE) `eq` id
prop_T_utf16BE           = (E.decodeUtf16BE . E.encodeUtf16BE) `eq` id
prop_T_utf32LE           = (E.decodeUtf32LE . E.encodeUtf32LE) `eq` id
prop_T_utf32BE           = (E.decodeUtf32BE . E.encodeUtf32BE) `eq` id

-- Do two functions give the same answer?
eq :: (Eq a) => (t -> a) -> (t -> a) -> t -> Bool
eq a b s  = crashy False $ a s == b s
-- What about with the RHS packed?
eqP :: (Eq a, Show a) => (String -> a) -> (T.Text -> a) -> String -> Word8 -> Bool
eqP a b s w  = eq "orig" (a s) (b t) &&
               eq "head" (a sa) (b ta) &&
               eq "tail" (a sb) (b tb)
    where t             = T.pack s
          (sa,sb)       = splitAt m s
          (ta,tb)       = T.splitAt m t
          l             = length s
          m | l == 0    = n
            | otherwise = n `mod` l
          n             = fromIntegral w
          eq s a b | crashy False $ a == b = True
                   | otherwise = trace (s ++ ": " ++ show a ++ " /= " ++ show b) False
-- Or with the string non-empty, and the RHS packed?
eqEP :: (Eq a) =>
        (String -> a) -> (T.Text -> a) -> NotEmpty String -> Word8 -> Bool
eqEP a b e w  = a s == b t &&
                (null sa || a sa == b ta) &&
                (null sb || a sb == b tb)
    where (sa,sb)       = splitAt m s
          (ta,tb)       = T.splitAt m t
          t             = T.pack s
          l             = length s
          m | l == 0    = n
            | otherwise = n `mod` l
          n             = fromIntegral w
          s             = notEmpty e

prop_T_cons x          = (x:)     `eqP` (T.unpack . T.cons x)
prop_T_snoc x          = (++ [x]) `eqP` (T.unpack . (flip T.snoc) x)
prop_T_append s        = (s++)    `eqP` (T.unpack . T.append (T.pack s))
prop_T_appendS s       = (s++)    `eqP` (T.unpack . S.unstream . S.append (S.stream (T.pack s)) . S.stream)
prop_T_uncons s        = uncons   `eqP` (fmap (second T.unpack) . T.uncons)
    where uncons (x:xs) = Just (x,xs)
          uncons _      = Nothing
          types         = s :: String
prop_T_head            = head   `eqEP` T.head
prop_T_last            = last   `eqEP` T.last
prop_T_lastS           = last   `eqEP` (S.last . S.stream)
prop_T_tail            = tail   `eqEP` (T.unpack . T.tail)
prop_T_tailS           = tail   `eqEP` (T.unpack . S.unstream . S.tail . S.stream)
prop_T_init            = init   `eqEP` (T.unpack . T.init)
prop_T_initS           = init   `eqEP` (T.unpack . S.unstream . S.init . S.stream)
prop_T_null            = null   `eqP`  T.null
prop_T_length          = length `eqP`  T.length
prop_T_map f           = map f  `eqP`  (T.unpack . T.map f)
prop_T_intercalate c   = L.intercalate c `eq` (T.unpack . T.intercalate (T.pack c) . map T.pack)
prop_T_intersperse c   = L.intersperse c `eqP` (T.unpack . T.intersperse c)
prop_T_transpose       = L.transpose `eq` (map T.unpack . T.transpose . map T.pack)
prop_T_reverse         = L.reverse `eqP` (T.unpack . T.reverse)
prop_T_reverse_short n = L.reverse `eqP` (T.unpack . S.reverse . shorten n . S.stream)

prop_T_foldl f z       = L.foldl f z  `eqP`  (T.foldl f z)
    where types      = f :: Char -> Char -> Char
prop_T_foldl' f z      = L.foldl' f z `eqP`  T.foldl' f z
    where types      = f :: Char -> Char -> Char
prop_T_foldl1 f        = L.foldl1 f   `eqEP` T.foldl1 f
prop_T_foldl1' f       = L.foldl1' f  `eqEP` T.foldl1' f
prop_T_foldr f z       = L.foldr f z  `eqP`  T.foldr f z
    where types      = f :: Char -> Char -> Char
prop_T_foldr1 f        = L.foldr1 f   `eqEP` T.foldr1 f

prop_T_concat          = L.concat      `eq`   (T.unpack . T.concat . map T.pack)
prop_T_concatMap f     = L.concatMap f `eqP`  (T.unpack . T.concatMap (T.pack . f))
prop_T_any p           = L.any p       `eqP`  T.any p
prop_T_all p           = L.all p       `eqP`  T.all p
prop_T_maximum         = L.maximum     `eqEP` T.maximum
prop_T_minimum         = L.minimum     `eqEP` T.minimum

prop_T_scanl f z       = L.scanl f z   `eqP`  (T.unpack . T.scanl f z)
prop_T_scanl1 f        = L.scanl1 f    `eqP`  (T.unpack . T.scanl1 f)
prop_T_scanr f z       = L.scanr f z   `eqP`  (T.unpack . T.scanr f z)
prop_T_scanr1 f        = L.scanr1 f    `eqP`  (T.unpack . T.scanr1 f)

prop_T_mapAccumL f z   = L.mapAccumL f z `eqP` (second T.unpack . T.mapAccumL f z)
    where types = f :: Int -> Char -> (Int,Char)
prop_T_mapAccumR f z   = L.mapAccumR f z `eqP` (second T.unpack . T.mapAccumR f z)
    where types = f :: Int -> Char -> (Int,Char)

prop_T_replicate n     = L.replicate n `eq`   (T.unpack . T.replicate n)
prop_T_unfoldr n       = L.unfoldr f   `eq`   (T.unpack . T.unfoldr f)
    where f c | fromEnum c * 100 > n = Nothing
              | otherwise            = Just (c, succ c)

prop_T_unfoldrN n m    = (L.take n . L.unfoldr f) `eq` (T.unpack . T.unfoldrN n f)
    where f c | fromEnum c * 100 > m = Nothing
              | otherwise            = Just (c, succ c)

unpack2 = T.unpack *** T.unpack

prop_T_take n          = L.take n      `eqP` (T.unpack . T.take n)
prop_T_drop n          = L.drop n      `eqP` (T.unpack . T.drop n)
prop_T_takeWhile p     = L.takeWhile p `eqP` (T.unpack . T.takeWhile p)
prop_T_takeWhileS p    = L.takeWhile p `eqP` (T.unpack . S.unstream . S.takeWhile p . S.stream)
prop_T_dropWhile p     = L.dropWhile p `eqP` (T.unpack . T.dropWhile p)
prop_T_dropWhileS p    = L.dropWhile p `eqP` (T.unpack . S.unstream . S.dropWhile p . S.stream)
prop_T_splitAt n       = L.splitAt n   `eqP` (unpack2 . T.splitAt n)
prop_T_span p          = L.span p      `eqP` (unpack2 . T.span p)
prop_T_break p         = L.break p     `eqP` (unpack2 . T.break p)
prop_T_group           = L.group       `eqP` (map T.unpack . T.group)
prop_T_groupBy p       = L.groupBy p   `eqP` (map T.unpack . T.groupBy p)
prop_T_inits           = L.inits       `eqP` (map T.unpack . T.inits)
prop_T_tails           = L.tails       `eqP` (map T.unpack . T.tails)

prop_T_split_i c       = id `eq` (T.intercalate (T.singleton c) . T.split c)

prop_T_splitWith p     = splitWith p `eqP` (map T.unpack . T.splitWith p)

splitWith _ "" =  []
splitWith p s  = if null s'
                 then [s]
                 else l : splitWith p (tail s')
    where (l, s') = break p s

prop_T_breakSubstring_isInfixOf s l
                     = T.isInfixOf s l ==
                       T.null s || (not . T.null . snd $ T.breakSubstring s l)
prop_T_breakSubstringC c
                     = L.break (==c) `eqP`
                       (unpack2 . T.breakSubstring (T.singleton c))

prop_T_lines           = L.lines       `eqP` (map T.unpack . T.lines)
{-
prop_T_lines'          = lines'        `eqP` (map T.unpack . T.lines')
    where lines' "" =  []
          lines' s =  let (l, s') = break eol s
                      in  l : case s' of
                                []      -> []
                                ('\r':'\n':s'') -> lines' s''
                                (_:s'') -> lines' s''
          eol c = c == '\r' || c == '\n'
-}
prop_T_words           = L.words       `eqP` (map T.unpack . T.words)
prop_T_unlines         = L.unlines     `eq`  (T.unpack . T.unlines . map T.pack)
prop_T_unwords         = L.unwords     `eq`  (T.unpack . T.unwords . map T.pack)

prop_T_isPrefixOf s    = L.isPrefixOf s`eqP` T.isPrefixOf (T.pack s)
prop_T_isPrefixOfS s   = L.isPrefixOf s`eqP` (S.isPrefixOf (S.stream $ T.pack s) . S.stream)
prop_T_isSuffixOf s    = L.isSuffixOf s`eqP` T.isSuffixOf (T.pack s)
prop_T_isInfixOf s     = L.isInfixOf s `eqP` T.isInfixOf (T.pack s)

prop_T_elem c          = L.elem c      `eqP` T.elem c
prop_T_filter p        = L.filter p    `eqP` (T.unpack . T.filter p)
prop_T_find p          = L.find p      `eqP` T.find p
prop_T_partition p     = L.partition p `eqP` (unpack2 . T.partition p)

prop_T_index x s       = x < L.length s && x >= 0 ==>
                       (L.!!) s x == T.index (T.pack s) x
prop_T_findIndex p     = L.findIndex p `eqP` T.findIndex p
prop_T_findIndices p   = L.findIndices p`eqP` T.findIndices p
prop_T_elemIndex c     = L.elemIndex c `eqP` T.elemIndex c
prop_T_elemIndices c   = L.elemIndices c`eqP` T.elemIndices c
prop_T_count c         = (L.length . L.elemIndices c) `eqP` T.count c
prop_T_zipWith c s     = L.zipWith c s `eqP` (T.unpack . T.zipWith c (T.pack s))

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
  ("prop_T_pack_unpack", mytest prop_T_pack_unpack),
  ("prop_TL_pack_unpack", mytest prop_TL_pack_unpack),
  ("prop_T_stream_unstream", mytest prop_T_stream_unstream),
  ("prop_TL_stream_unstream", mytest prop_TL_stream_unstream),
  ("prop_T_reverse_stream", mytest prop_T_reverse_stream),
  ("prop_T_singleton", mytest prop_T_singleton),

  ("prop_T_ascii", mytest prop_T_ascii),
  ("prop_T_utf8", mytest prop_T_utf8),
  ("prop_T_utf16LE", mytest prop_T_utf16LE),
  ("prop_T_utf16BE", mytest prop_T_utf16BE),
  ("prop_T_utf32LE", mytest prop_T_utf32LE),
  ("prop_T_utf32BE", mytest prop_T_utf32BE),

  ("prop_T_cons", mytest prop_T_cons),
  ("prop_T_snoc", mytest prop_T_snoc),
  ("prop_T_append", mytest prop_T_append),
  ("prop_T_appendS", mytest prop_T_appendS),
  ("prop_T_uncons", mytest prop_T_uncons),
  ("prop_T_head", mytest prop_T_head),
  ("prop_T_last", mytest prop_T_last),
  ("prop_T_lastS", mytest prop_T_lastS),
  ("prop_T_tail", mytest prop_T_tail),
  ("prop_T_tailS", mytest prop_T_tailS),
  ("prop_T_init", mytest prop_T_init),
  ("prop_T_initS", mytest prop_T_initS),
  ("prop_T_null", mytest prop_T_null),
  ("prop_T_length", mytest prop_T_length),

  ("prop_T_map", mytest prop_T_map),
  ("prop_T_intercalate", mytest prop_T_intercalate),
  ("prop_T_intersperse", mytest prop_T_intersperse),
  ("prop_T_transpose", mytest prop_T_transpose),
  ("prop_T_reverse", mytest prop_T_reverse),
  ("prop_T_reverse_short", mytest prop_T_reverse_short),

  ("prop_T_foldl", mytest prop_T_foldl),
  ("prop_T_foldl'", mytest prop_T_foldl'),
  ("prop_T_foldl1", mytest prop_T_foldl1),
  ("prop_T_foldl1'", mytest prop_T_foldl1'),
  ("prop_T_foldr", mytest prop_T_foldr),
  ("prop_T_foldr1", mytest prop_T_foldr1),

  ("prop_T_concat", mytest prop_T_concat),
  ("prop_T_concatMap", mytest prop_T_concatMap),
  ("prop_T_any", mytest prop_T_any),
  ("prop_T_all", mytest prop_T_all),
  ("prop_T_maximum", mytest prop_T_maximum),
  ("prop_T_minimum", mytest prop_T_minimum),

  ("prop_T_scanl", mytest prop_T_scanl),
  ("prop_T_scanl1", mytest prop_T_scanl1),
  ("prop_T_scanr", mytest prop_T_scanr),
  ("prop_T_scanr1", mytest prop_T_scanr1),

  ("prop_T_mapAccumL", mytest prop_T_mapAccumL),
  ("prop_T_mapAccumR", mytest prop_T_mapAccumR),

  ("prop_T_replicate", mytest prop_T_replicate),
  ("prop_T_unfoldr", mytest prop_T_unfoldr),
  ("prop_T_unfoldrN", mytest prop_T_unfoldrN),

  ("prop_T_take", mytest prop_T_take),
  ("prop_T_drop", mytest prop_T_drop),
  ("prop_T_takeWhile", mytest prop_T_takeWhile),
  ("prop_T_takeWhileS", mytest prop_T_takeWhileS),
  ("prop_T_dropWhile", mytest prop_T_dropWhile),
  ("prop_T_dropWhileS", mytest prop_T_dropWhileS),
  ("prop_T_splitAt", mytest prop_T_splitAt),
  ("prop_T_span", mytest prop_T_span),
  ("prop_T_break", mytest prop_T_break),
  ("prop_T_group", mytest prop_T_group),
  ("prop_T_groupBy", mytest prop_T_groupBy),
  ("prop_T_inits", mytest prop_T_inits),
  ("prop_T_tails", mytest prop_T_tails),

  ("prop_T_split_i", mytest prop_T_split_i),
  ("prop_T_splitWith", mytest prop_T_splitWith),
  ("prop_T_breakSubstringC", mytest prop_T_breakSubstringC),
  ("prop_T_breakSubstring_isInfixOf", mytest prop_T_breakSubstring_isInfixOf),

  ("prop_T_lines", mytest prop_T_lines),
--("prop_T_lines'", mytest prop_T_lines'),
  ("prop_T_words", mytest prop_T_words),
  ("prop_T_unlines", mytest prop_T_unlines),
  ("prop_T_unwords", mytest prop_T_unwords),

  ("prop_T_isPrefixOf", mytest prop_T_isPrefixOf),
  ("prop_T_isPrefixOfS", mytest prop_T_isPrefixOfS),
  ("prop_T_isSuffixOf", mytest prop_T_isSuffixOf),
  ("prop_T_isInfixOf", mytest prop_T_isInfixOf),

  ("prop_T_elem", mytest prop_T_elem),
  ("prop_T_filter", mytest prop_T_filter),
  ("prop_T_find", mytest prop_T_find),
  ("prop_T_partition", mytest prop_T_partition),

  ("prop_T_index", mytest prop_T_index),
  ("prop_T_findIndex", mytest prop_T_findIndex),
  ("prop_T_findIndices", mytest prop_T_findIndices),
  ("prop_T_elemIndex", mytest prop_T_elemIndex),
  ("prop_T_elemIndices", mytest prop_T_elemIndices),
  ("prop_T_count", mytest prop_T_count),
  ("prop_T_zipWith", mytest prop_T_zipWith)
  ]
