{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
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
import qualified Data.Text.Fusion.Common as S
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
prop_TL_singleton c      = [c] == (TL.unpack . TL.singleton) c
prop_TL_unstreamChunks x = f 11 x == f 1000 x
    where f n = SL.unstreamChunks n . S.streamList
prop_TL_chunk_unchunk    = (TL.fromChunks . TL.toChunks) `eq` id

prop_T_ascii t           = E.decodeASCII (E.encodeUtf8 a) == a
    where a            = T.map (\c -> chr (ord c `mod` 128)) t
prop_T_utf8              = (E.decodeUtf8 . E.encodeUtf8) `eq` id
prop_T_utf16LE           = (E.decodeUtf16LE . E.encodeUtf16LE) `eq` id
prop_T_utf16BE           = (E.decodeUtf16BE . E.encodeUtf16BE) `eq` id
prop_T_utf32LE           = (E.decodeUtf32LE . E.encodeUtf32LE) `eq` id
prop_T_utf32BE           = (E.decodeUtf32BE . E.encodeUtf32BE) `eq` id

class Target t where
    packT    :: String -> t
    unpackT  :: t -> String
    splitAtT :: Int -> t -> (t,t)
    packTChunkSize :: Int -> String -> t
    packTChunkSize _ = packT

instance Target String where
    packT    = id
    unpackT  = id
    splitAtT = splitAt

instance Target (S.Stream Char) where
    packT        = S.streamList
    unpackT      = S.unstreamList
    splitAtT n s = (S.take n s, S.drop n s)

instance Target T.Text where
    packT    = T.pack
    unpackT  = T.unpack
    splitAtT = T.splitAt

instance Target TL.Text where
    packTChunkSize k = SL.unstreamChunks k . S.streamList
    packT    = TL.pack
    unpackT  = TL.unpack
    splitAtT = TL.splitAt . fromIntegral

-- Do two functions give the same answer?
eq :: (Eq a) => (t -> a) -> (t -> a) -> t -> Bool
eq a b s  = crashy False $ a s == b s

-- What about with the RHS packed?
eqP :: (Eq a, Show a, Target t) =>
       (String -> a) -> (t -> a) -> String -> Word8 -> Bool
eqP a b s w  = eq "orig" (a s) (b t) &&
               eq "mini" (a s) (b mini) &&
               eq "head" (a sa) (b ta) &&
               eq "tail" (a sb) (b tb)
    where t             = packT s
          mini          = packTChunkSize 10 s
          (sa,sb)       = splitAt m s
          (ta,tb)       = splitAtT m t
          l             = length s
          m | l == 0    = n
            | otherwise = n `mod` l
          n             = fromIntegral w
          eq s a b | crashy False $ a == b = True
                   | otherwise = trace (s ++ ": " ++ show a ++ " /= " ++ show b) False

-- Or with the string non-empty, and the RHS packed?
eqEP :: (Eq a, Target t) =>
        (String -> a) -> (t -> a) -> NotEmpty String -> Word8 -> Bool
eqEP a b e w  = a s == b t &&
                a s == b mini &&
                (null sa || a sa == b ta) &&
                (null sb || a sb == b tb)
    where (sa,sb)       = splitAt m s
          (ta,tb)       = splitAtT m t
          t             = packT s
          mini          = packTChunkSize 10 s
          l             = length s
          m | l == 0    = n
            | otherwise = n `mod` l
          n             = fromIntegral w
          s             = notEmpty e

prop_S_cons x          = (x:)     `eqP` (unpackT . S.cons x)
prop_T_cons x          = (x:)     `eqP` (unpackT . T.cons x)
prop_TL_cons x         = ((x:)     `eqP` (TL.unpack . TL.cons x))
prop_S_snoc x          = (++ [x]) `eqP` (unpackT . (flip S.snoc) x)
prop_T_snoc x          = (++ [x]) `eqP` (unpackT . (flip T.snoc) x)
prop_TL_snoc x         = (++ [x]) `eqP` (unpackT . (flip TL.snoc) x)
prop_T_append s        = (s++)    `eqP` (unpackT . T.append (packT s))
prop_T_appendS s       = (s++)    `eqP` (unpackT . S.unstream . S.append (S.stream (packT s)) . S.stream)

uncons (x:xs) = Just (x,xs)
uncons _      = Nothing

prop_T_uncons s        = uncons   `eqP` (fmap (second unpackT) . T.uncons)
    where types = s :: String
prop_TL_uncons s       = uncons   `eqP` (fmap (second unpackT) . TL.uncons)
    where types = s :: String
prop_S_head            = head   `eqEP` S.head
prop_T_head            = head   `eqEP` T.head
prop_TL_head           = head   `eqEP` TL.head
prop_S_last            = last   `eqEP` S.last
prop_T_last            = last   `eqEP` T.last
prop_TL_last           = last   `eqEP` TL.last
prop_S_tail            = tail   `eqEP` (unpackT . S.tail)
prop_T_tail            = tail   `eqEP` (unpackT . T.tail)
prop_TL_tail           = tail   `eqEP` (unpackT . TL.tail)
prop_S_init            = init   `eqEP` (unpackT . S.init)
prop_T_init            = init   `eqEP` (unpackT . T.init)
prop_TL_init           = init   `eqEP` (unpackT . TL.init)
prop_S_null            = null   `eqP`  S.null
prop_T_null            = null   `eqP`  T.null
prop_TL_null           = null   `eqP`  TL.null
prop_S_length          = length `eqP`  S.length
prop_T_length          = length `eqP`  T.length
prop_TL_length         = length `eqP`  (fromIntegral . TL.length)
prop_T_map f           = map f  `eqP`  (unpackT . T.map f)
prop_TL_map f          = map f  `eqP`  (unpackT . TL.map f)
prop_T_intercalate c   = L.intercalate c `eq` (unpackT . T.intercalate (packT c) . map packT)
prop_TL_intercalate c  = L.intercalate c `eq` (unpackT . TL.intercalate (TL.pack c) . map TL.pack)
prop_T_intersperse c   = L.intersperse c `eqP` (unpackT . T.intersperse c)
prop_TL_intersperse c  = L.intersperse c `eqP` (unpackT . TL.intersperse c)
prop_T_transpose       = L.transpose `eq` (map unpackT . T.transpose . map packT)
prop_TL_transpose      = L.transpose `eq` (map unpackT . TL.transpose . map TL.pack)
prop_T_reverse         = L.reverse `eqP` (unpackT . T.reverse)
prop_TL_reverse        = L.reverse `eqP` (unpackT . TL.reverse)
prop_T_reverse_short n = L.reverse `eqP` (unpackT . S.reverse . shorten n . S.stream)

prop_T_foldl f z       = L.foldl f z  `eqP`  (T.foldl f z)
    where types      = f :: Char -> Char -> Char
prop_TL_foldl f z      = L.foldl f z  `eqP`  (TL.foldl f z)
    where types      = f :: Char -> Char -> Char
prop_T_foldl' f z      = L.foldl' f z `eqP`  T.foldl' f z
    where types      = f :: Char -> Char -> Char
prop_TL_foldl' f z     = L.foldl' f z `eqP`  TL.foldl' f z
    where types      = f :: Char -> Char -> Char
prop_T_foldl1 f        = L.foldl1 f   `eqEP` T.foldl1 f
prop_TL_foldl1 f       = L.foldl1 f   `eqEP` TL.foldl1 f
prop_T_foldl1' f       = L.foldl1' f  `eqEP` T.foldl1' f
prop_TL_foldl1' f      = L.foldl1' f  `eqEP` TL.foldl1' f
prop_T_foldr f z       = L.foldr f z  `eqP`  T.foldr f z
    where types      = f :: Char -> Char -> Char
prop_TL_foldr f z      = L.foldr f z  `eqP`  TL.foldr f z
    where types      = f :: Char -> Char -> Char
prop_T_foldr1 f        = L.foldr1 f   `eqEP` T.foldr1 f
prop_TL_foldr1 f       = L.foldr1 f   `eqEP` TL.foldr1 f

prop_T_concat          = L.concat      `eq`   (unpackT . T.concat . map packT)
prop_TL_concat         = L.concat      `eq`   (unpackT . TL.concat . map TL.pack)
prop_T_concatMap f     = L.concatMap f `eqP`  (unpackT . T.concatMap (packT . f))
prop_TL_concatMap f    = L.concatMap f `eqP`  (unpackT . TL.concatMap (TL.pack . f))
prop_T_any p           = L.any p       `eqP`  T.any p
prop_TL_any p          = L.any p       `eqP`  TL.any p
prop_T_all p           = L.all p       `eqP`  T.all p
prop_TL_all p          = L.all p       `eqP`  TL.all p
prop_T_maximum         = L.maximum     `eqEP` T.maximum
prop_TL_maximum        = L.maximum     `eqEP` TL.maximum
prop_T_minimum         = L.minimum     `eqEP` T.minimum
prop_TL_minimum        = L.minimum     `eqEP` TL.minimum

prop_T_scanl f z       = L.scanl f z   `eqP`  (unpackT . T.scanl f z)
prop_TL_scanl f z      = L.scanl f z   `eqP`  (unpackT . TL.scanl f z)
prop_T_scanl1 f        = L.scanl1 f    `eqP`  (unpackT . T.scanl1 f)
prop_TL_scanl1 f       = L.scanl1 f    `eqP`  (unpackT . TL.scanl1 f)
prop_T_scanr f z       = L.scanr f z   `eqP`  (unpackT . T.scanr f z)
prop_TL_scanr f z      = L.scanr f z   `eqP`  (unpackT . TL.scanr f z)
prop_T_scanr1 f        = L.scanr1 f    `eqP`  (unpackT . T.scanr1 f)
prop_TL_scanr1 f       = L.scanr1 f    `eqP`  (unpackT . TL.scanr1 f)

prop_T_mapAccumL f z   = L.mapAccumL f z `eqP` (second unpackT . T.mapAccumL f z)
    where types = f :: Int -> Char -> (Int,Char)
prop_TL_mapAccumL f z  = L.mapAccumL f z `eqP` (second unpackT . TL.mapAccumL f z)
    where types = f :: Int -> Char -> (Int,Char)
prop_T_mapAccumR f z   = L.mapAccumR f z `eqP` (second unpackT . T.mapAccumR f z)
    where types = f :: Int -> Char -> (Int,Char)
prop_TL_mapAccumR f z   = L.mapAccumR f z `eqP` (second unpackT . TL.mapAccumR f z)
    where types = f :: Int -> Char -> (Int,Char)

prop_T_replicate n     = L.replicate n `eq` (unpackT . T.replicate n)
prop_TL_replicate n    = L.replicate n `eq` (unpackT . TL.replicate n)

unf :: Int -> Char -> Maybe (Char, Char)
unf n c | fromEnum c * 100 > n = Nothing
        | otherwise            = Just (c, succ c)

prop_T_unfoldr n       = L.unfoldr (unf n) `eq` (unpackT . T.unfoldr (unf n))
prop_TL_unfoldr n      = L.unfoldr (unf n) `eq` (unpackT . TL.unfoldr (unf n))
prop_T_unfoldrN n m    = (L.take n . L.unfoldr (unf m)) `eq`
                         (unpackT . T.unfoldrN n (unf m))
prop_TL_unfoldrN n m   = (L.take n . L.unfoldr (unf m)) `eq`
                         (unpackT . TL.unfoldrN (fromIntegral n) (unf m))

unpack2 :: (Target t) => (t,t) -> (String,String)
unpack2 = unpackT *** unpackT

prop_S_take n          = L.take n      `eqP` (unpackT . S.take n)
prop_T_take n          = L.take n      `eqP` (unpackT . T.take n)
prop_TL_take n         = L.take n      `eqP` (unpackT . TL.take (fromIntegral n))
prop_S_drop n          = L.drop n      `eqP` (unpackT . S.drop n)
prop_T_drop n          = L.drop n      `eqP` (unpackT . T.drop n)
prop_TL_drop n         = L.drop n      `eqP` (unpackT . TL.drop n)
prop_S_takeWhile p     = L.takeWhile p `eqP` (unpackT . S.takeWhile p)
prop_T_takeWhile p     = L.takeWhile p `eqP` (unpackT . T.takeWhile p)
prop_TL_takeWhile p    = L.takeWhile p `eqP` (unpackT . TL.takeWhile p)
prop_S_dropWhile p     = L.dropWhile p `eqP` (unpackT . S.dropWhile p)
prop_T_dropWhile p     = L.dropWhile p `eqP` (unpackT . T.dropWhile p)
prop_TL_dropWhile p    = L.dropWhile p `eqP` (unpackT . S.dropWhile p)
prop_T_splitAt n       = L.splitAt n   `eqP` (unpack2 . T.splitAt n)
prop_TL_splitAt n      = L.splitAt n   `eqP` (unpack2 . TL.splitAt (fromIntegral n))
prop_T_span p          = L.span p      `eqP` (unpack2 . T.span p)
prop_TL_span p         = L.span p      `eqP` (unpack2 . TL.span p)
prop_T_break p         = L.break p     `eqP` (unpack2 . T.break p)
prop_TL_break p        = L.break p     `eqP` (unpack2 . TL.break p)
prop_T_group           = L.group       `eqP` (map unpackT . T.group)
prop_TL_group          = L.group       `eqP` (map unpackT . TL.group)
prop_T_groupBy p       = L.groupBy p   `eqP` (map unpackT . T.groupBy p)
prop_TL_groupBy p      = L.groupBy p   `eqP` (map unpackT . TL.groupBy p)
prop_T_inits           = L.inits       `eqP` (map unpackT . T.inits)
prop_TL_inits          = L.inits       `eqP` (map unpackT . TL.inits)
prop_T_tails           = L.tails       `eqP` (map unpackT . T.tails)
prop_TL_tails          = L.tails       `eqP` (map unpackT . TL.tails)

prop_T_split_i c       = id `eq` (T.intercalate (T.singleton c) . T.split c)
prop_TL_split_i c      = id `eq` (TL.intercalate (TL.singleton c) . TL.split c)

prop_T_splitWith p     = splitWith p `eqP` (map unpackT . T.splitWith p)
prop_TL_splitWith p    = splitWith p `eqP` (map unpackT . TL.splitWith p)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] =  []
splitWith p xs = loop xs
    where loop s | null s'   = [l]
                 | otherwise = l : loop (tail s')
              where (l, s') = break p s

prop_T_breakSubstring_isInfixOf s l
                     = T.isInfixOf s l ==
                       T.null s || (not . T.null . snd $ T.breakSubstring s l)
prop_T_breakSubstringC c
                     = L.break (==c) `eqP`
                       (unpack2 . T.breakSubstring (T.singleton c))

prop_T_lines           = L.lines       `eqP` (map unpackT . T.lines)
prop_TL_lines          = L.lines       `eqP` (map unpackT . TL.lines)
{-
prop_T_lines'          = lines'        `eqP` (map unpackT . T.lines')
    where lines' "" =  []
          lines' s =  let (l, s') = break eol s
                      in  l : case s' of
                                []      -> []
                                ('\r':'\n':s'') -> lines' s''
                                (_:s'') -> lines' s''
          eol c = c == '\r' || c == '\n'
-}
prop_T_words           = L.words       `eqP` (map unpackT . T.words)
prop_TL_words          = L.words       `eqP` (map unpackT . TL.words)
prop_T_unlines         = L.unlines     `eq`  (unpackT . T.unlines . map packT)
prop_TL_unlines        = L.unlines     `eq`  (unpackT . TL.unlines . map packT)
prop_T_unwords         = L.unwords     `eq`  (unpackT . T.unwords . map packT)
prop_TL_unwords        = L.unwords     `eq`  (unpackT . TL.unwords . map packT)

prop_S_isPrefixOf s    = L.isPrefixOf s`eqP` (S.isPrefixOf (S.stream $ packT s) . S.stream)
prop_T_isPrefixOf s    = L.isPrefixOf s`eqP` T.isPrefixOf (packT s)
prop_TL_isPrefixOf s   = L.isPrefixOf s`eqP` TL.isPrefixOf (packT s)
prop_T_isSuffixOf s    = L.isSuffixOf s`eqP` T.isSuffixOf (packT s)
prop_TL_isSuffixOf s   = L.isSuffixOf s`eqP` TL.isSuffixOf (packT s)
prop_T_isInfixOf s     = L.isInfixOf s `eqP` T.isInfixOf (packT s)
prop_TL_isInfixOf s    = L.isInfixOf s `eqP` TL.isInfixOf (packT s)

prop_T_elem c          = L.elem c      `eqP` T.elem c
prop_TL_elem c         = L.elem c      `eqP` TL.elem c
prop_T_filter p        = L.filter p    `eqP` (unpackT . T.filter p)
prop_TL_filter p       = L.filter p    `eqP` (unpackT . TL.filter p)
prop_T_find p          = L.find p      `eqP` T.find p
prop_TL_find p         = L.find p      `eqP` TL.find p
prop_T_partition p     = L.partition p `eqP` (unpack2 . T.partition p)
prop_TL_partition p    = L.partition p `eqP` (unpack2 . TL.partition p)

prop_T_index x s       = x < L.length s && x >= 0 ==>
                         (L.!!) s x == T.index (packT s) x
prop_T_findIndex p     = L.findIndex p `eqP` T.findIndex p
prop_T_findIndices p   = L.findIndices p`eqP` T.findIndices p
prop_T_elemIndex c     = L.elemIndex c `eqP` T.elemIndex c
prop_T_elemIndices c   = L.elemIndices c`eqP` T.elemIndices c
prop_T_count c         = (L.length . L.elemIndices c) `eqP` T.count c
prop_T_zipWith c s     = L.zipWith c s `eqP` (unpackT . T.zipWith c (packT s))
prop_TL_zipWith c s    = L.zipWith c s `eqP` (unpackT . TL.zipWith c (packT s))

-- Regression tests.
prop_S_filter_eq s = S.filter p t == S.streamList (filter p s)
    where p = (/= S.last t)
          t = S.streamList s

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
  let (n,names) = case args of
                    (k:ts) -> (read k,ts)
                    []  -> (100,[])
  (results,passed) <- fmap unzip . forM tests $ \(s,a) ->
                      if null names || s `elem` names
                      then printf "%-40s: " s >> a n
                      else return (True,0)
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
  ("prop_TL_singleton", mytest prop_TL_singleton),
  ("prop_TL_unstreamChunks", mytest prop_TL_unstreamChunks),
  ("prop_TL_chunk_unchunk", mytest prop_TL_chunk_unchunk),

  ("prop_T_ascii", mytest prop_T_ascii),
  ("prop_T_utf8", mytest prop_T_utf8),
  ("prop_T_utf16LE", mytest prop_T_utf16LE),
  ("prop_T_utf16BE", mytest prop_T_utf16BE),
  ("prop_T_utf32LE", mytest prop_T_utf32LE),
  ("prop_T_utf32BE", mytest prop_T_utf32BE),

  ("prop_S_cons", mytest prop_S_cons),
  ("prop_T_cons", mytest prop_T_cons),
  ("prop_TL_cons", mytest prop_TL_cons),
  ("prop_S_snoc", mytest prop_S_snoc),
  ("prop_T_snoc", mytest prop_T_snoc),
  ("prop_TL_snoc", mytest prop_TL_snoc),
  ("prop_T_append", mytest prop_T_append),
  ("prop_T_appendS", mytest prop_T_appendS),
  ("prop_T_uncons", mytest prop_T_uncons),
  ("prop_TL_uncons", mytest prop_TL_uncons),
  ("prop_S_head", mytest prop_S_head),
  ("prop_T_head", mytest prop_T_head),
  ("prop_TL_head", mytest prop_TL_head),
  ("prop_S_last", mytest prop_S_last),
  ("prop_T_last", mytest prop_T_last),
  ("prop_TL_last", mytest prop_TL_last),
  ("prop_S_tail", mytest prop_S_tail),
  ("prop_T_tail", mytest prop_T_tail),
  ("prop_TL_tail", mytest prop_TL_tail),
  ("prop_S_init", mytest prop_S_init),
  ("prop_T_init", mytest prop_T_init),
  ("prop_TL_init", mytest prop_TL_init),
  ("prop_S_null", mytest prop_S_null),
  ("prop_T_null", mytest prop_T_null),
  ("prop_TL_null", mytest prop_TL_null),
  ("prop_S_length", mytest prop_S_length),
  ("prop_T_length", mytest prop_T_length),
  ("prop_TL_length", mytest prop_TL_length),

  ("prop_T_map", mytest prop_T_map),
  ("prop_TL_map", mytest prop_TL_map),
  ("prop_T_intercalate", mytest prop_T_intercalate),
  ("prop_TL_intercalate", mytest prop_TL_intercalate),
  ("prop_T_intersperse", mytest prop_T_intersperse),
  ("prop_TL_intersperse", mytest prop_TL_intersperse),
  ("prop_T_transpose", mytest prop_T_transpose),
  ("prop_TL_transpose", mytest prop_TL_transpose),
  ("prop_T_reverse", mytest prop_T_reverse),
  ("prop_TL_reverse", mytest prop_TL_reverse),
  ("prop_T_reverse_short", mytest prop_T_reverse_short),

  ("prop_T_foldl", mytest prop_T_foldl),
  ("prop_TL_foldl", mytest prop_TL_foldl),
  ("prop_T_foldl'", mytest prop_T_foldl'),
  ("prop_TL_foldl'", mytest prop_TL_foldl'),
  ("prop_T_foldl1", mytest prop_T_foldl1),
  ("prop_TL_foldl1", mytest prop_TL_foldl1),
  ("prop_T_foldl1'", mytest prop_T_foldl1'),
  ("prop_TL_foldl1'", mytest prop_TL_foldl1'),
  ("prop_T_foldr", mytest prop_T_foldr),
  ("prop_TL_foldr", mytest prop_TL_foldr),
  ("prop_T_foldr1", mytest prop_T_foldr1),
  ("prop_TL_foldr1", mytest prop_TL_foldr1),

  ("prop_T_concat", mytest prop_T_concat),
  ("prop_TL_concat", mytest prop_TL_concat),
  ("prop_T_concatMap", mytest prop_T_concatMap),
  ("prop_TL_concatMap", mytest prop_TL_concatMap),
  ("prop_T_any", mytest prop_T_any),
  ("prop_TL_any", mytest prop_TL_any),
  ("prop_T_all", mytest prop_T_all),
  ("prop_TL_all", mytest prop_TL_all),
  ("prop_T_maximum", mytest prop_T_maximum),
  ("prop_TL_maximum", mytest prop_TL_maximum),
  ("prop_T_minimum", mytest prop_T_minimum),
  ("prop_TL_minimum", mytest prop_TL_minimum),

  ("prop_T_scanl", mytest prop_T_scanl),
  ("prop_TL_scanl", mytest prop_TL_scanl),
  ("prop_T_scanl1", mytest prop_T_scanl1),
  ("prop_TL_scanl1", mytest prop_TL_scanl1),
  ("prop_T_scanr", mytest prop_T_scanr),
  ("prop_TL_scanr", mytest prop_TL_scanr),
  ("prop_T_scanr1", mytest prop_T_scanr1),
  ("prop_TL_scanr1", mytest prop_TL_scanr1),

  ("prop_T_mapAccumL", mytest prop_T_mapAccumL),
  ("prop_TL_mapAccumL", mytest prop_TL_mapAccumL),
  ("prop_T_mapAccumR", mytest prop_T_mapAccumR),
  ("prop_TL_mapAccumR", mytest prop_TL_mapAccumR),

  ("prop_T_replicate", mytest prop_T_replicate),
  ("prop_TL_replicate", mytest prop_TL_replicate),
  ("prop_T_unfoldr", mytest prop_T_unfoldr),
  ("prop_TL_unfoldr", mytest prop_TL_unfoldr),
  ("prop_T_unfoldrN", mytest prop_T_unfoldrN),
  ("prop_TL_unfoldrN", mytest prop_TL_unfoldrN),

  ("prop_S_take", mytest prop_S_take),
  ("prop_T_take", mytest prop_T_take),
  ("prop_TL_take", mytest prop_TL_take),
  ("prop_S_drop", mytest prop_S_drop),
  ("prop_T_drop", mytest prop_T_drop),
  ("prop_TL_drop", mytest prop_TL_drop),
  ("prop_S_takeWhile", mytest prop_S_takeWhile),
  ("prop_T_takeWhile", mytest prop_T_takeWhile),
  ("prop_TL_takeWhile", mytest prop_TL_takeWhile),
  ("prop_S_dropWhile", mytest prop_S_dropWhile),
  ("prop_T_dropWhile", mytest prop_T_dropWhile),
  ("prop_TL_dropWhile", mytest prop_TL_dropWhile),
  ("prop_T_splitAt", mytest prop_T_splitAt),
  ("prop_T_span", mytest prop_T_span),
  ("prop_TL_span", mytest prop_TL_span),
  ("prop_T_break", mytest prop_T_break),
  ("prop_TL_break", mytest prop_TL_break),
  ("prop_T_group", mytest prop_T_group),
  ("prop_TL_group", mytest prop_TL_group),
  ("prop_T_groupBy", mytest prop_T_groupBy),
  ("prop_TL_groupBy", mytest prop_TL_groupBy),
  ("prop_T_inits", mytest prop_T_inits),
  ("prop_TL_inits", mytest prop_TL_inits),
  ("prop_T_tails", mytest prop_T_tails),
  ("prop_TL_tails", mytest prop_TL_tails),

  ("prop_T_split_i", mytest prop_T_split_i),
  ("prop_TL_split_i", mytest prop_TL_split_i),
  ("prop_T_splitWith", mytest prop_T_splitWith),
  ("prop_TL_splitWith", mytest prop_TL_splitWith),
  ("prop_T_breakSubstringC", mytest prop_T_breakSubstringC),
  ("prop_T_breakSubstring_isInfixOf", mytest prop_T_breakSubstring_isInfixOf),

  ("prop_T_lines", mytest prop_T_lines),
  ("prop_TL_lines", mytest prop_TL_lines),
--("prop_T_lines'", mytest prop_T_lines'),
  ("prop_T_words", mytest prop_T_words),
  ("prop_TL_words", mytest prop_TL_words),
  ("prop_T_unlines", mytest prop_T_unlines),
  ("prop_TL_unlines", mytest prop_TL_unlines),
  ("prop_TL_unwords", mytest prop_TL_unwords),

  ("prop_S_isPrefixOf", mytest prop_S_isPrefixOf),
  ("prop_T_isPrefixOf", mytest prop_T_isPrefixOf),
  ("prop_TL_isPrefixOf", mytest prop_TL_isPrefixOf),
  ("prop_T_isSuffixOf", mytest prop_T_isSuffixOf),
  ("prop_TL_isSuffixOf", mytest prop_TL_isSuffixOf),
  ("prop_T_isInfixOf", mytest prop_T_isInfixOf),
  ("prop_TL_isInfixOf", mytest prop_TL_isInfixOf),

  ("prop_T_elem", mytest prop_T_elem),
  ("prop_TL_elem", mytest prop_TL_elem),
  ("prop_T_filter", mytest prop_T_filter),
  ("prop_TL_filter", mytest prop_TL_filter),
  ("prop_T_find", mytest prop_T_find),
  ("prop_TL_find", mytest prop_TL_find),
  ("prop_T_partition", mytest prop_T_partition),
  ("prop_TL_partition", mytest prop_TL_partition),

  ("prop_T_index", mytest prop_T_index),
  ("prop_T_findIndex", mytest prop_T_findIndex),
  ("prop_T_findIndices", mytest prop_T_findIndices),
  ("prop_T_elemIndex", mytest prop_T_elemIndex),
  ("prop_T_elemIndices", mytest prop_T_elemIndices),
  ("prop_T_count", mytest prop_T_count),
  ("prop_T_zipWith", mytest prop_T_zipWith),
  ("prop_TL_zipWith", mytest prop_TL_zipWith),

  ("prop_S_filter_eq", mytest prop_S_filter_eq)
  ]
