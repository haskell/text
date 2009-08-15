{-# LANGUAGE BangPatterns, FlexibleInstances, ScopedTypeVariables,
             TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

import Test.QuickCheck
import Text.Show.Functions ()

import Data.Char (chr, isLower, isSpace, isUpper, ord)
import Data.Monoid (Monoid(..))
import Data.String (fromString)
import Debug.Trace (trace)
import Control.Arrow ((***), second)
import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.Compat as C
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as E
import Control.Exception (SomeException, try)
import qualified Data.Text.Fusion as S
import qualified Data.Text.Fusion.Common as S
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.Text.Lazy.Fusion as SL
import qualified Data.List as L
import System.IO.Unsafe (unsafePerformIO)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)

import QuickCheckUtils ()

-- Ensure that two potentially bottom values (in the sense of crashing
-- for some inputs, not looping infinitely) either both crash, or both
-- give comparable results for some input.
(=^=) :: (Eq a, Show a) => a -> a -> Bool
{-# NOINLINE (=^=) #-}
i =^= j = unsafePerformIO $ do
  x <- try (return $! i)
  y <- try (return $! j)
  case (x,y) of
    (Left (_ :: SomeException), Left (_ :: SomeException))
                       -> return True
    (Right a, Right b) -> return (a == b)
    e                  -> trace ("*** Divergence: " ++ show e) return False
infix 4 =^=

t_pack_unpack       = (T.unpack . T.pack) `eq` id
tl_pack_unpack      = (TL.unpack . TL.pack) `eq` id
t_stream_unstream   = (S.unstream . S.stream) `eq` id
tl_stream_unstream  = (SL.unstream . SL.stream) `eq` id
t_reverse_stream t  = (S.reverse . S.reverseStream) t == t
t_singleton c       = [c] == (T.unpack . T.singleton) c
tl_singleton c      = [c] == (TL.unpack . TL.singleton) c
tl_unstreamChunks x = f 11 x == f 1000 x
    where f n = SL.unstreamChunks n . S.streamList
tl_chunk_unchunk    = (TL.fromChunks . TL.toChunks) `eq` id

t_ascii t           = E.decodeASCII (E.encodeUtf8 a) == a
    where a              = T.map (\c -> chr (ord c `mod` 128)) t
t_utf8              = (E.decodeUtf8 . E.encodeUtf8) `eq` id
tl_utf8             = (EL.decodeUtf8 . EL.encodeUtf8) `eq` id
t_utf16LE           = (E.decodeUtf16LE . E.encodeUtf16LE) `eq` id
t_utf16BE           = (E.decodeUtf16BE . E.encodeUtf16BE) `eq` id
t_utf32LE           = (E.decodeUtf32LE . E.encodeUtf32LE) `eq` id
t_utf32BE           = (E.decodeUtf32BE . E.encodeUtf32BE) `eq` id

class Stringy s where
    packS    :: String -> s
    unpackS  :: s -> String
    splitAtS :: Int -> s -> (s,s)
    packSChunkSize :: Int -> String -> s
    packSChunkSize _ = packS

instance Stringy String where
    packS    = id
    unpackS  = id
    splitAtS = splitAt

instance Stringy (S.Stream Char) where
    packS        = S.streamList
    unpackS      = S.unstreamList
    splitAtS n s = (S.take n s, S.drop n s)

instance Stringy T.Text where
    packS    = T.pack
    unpackS  = T.unpack
    splitAtS = T.splitAt

instance Stringy TL.Text where
    packSChunkSize k = SL.unstreamChunks k . S.streamList
    packS    = TL.pack
    unpackS  = TL.unpack
    splitAtS = TL.splitAt . fromIntegral

-- Do two functions give the same answer?
eq :: (Eq a, Show a) => (t -> a) -> (t -> a) -> t -> Bool
eq a b s  = a s =^= b s

-- What about with the RHS packed?
eqP :: (Eq a, Show a, Stringy s) =>
       (String -> a) -> (s -> a) -> String -> Word8 -> Bool
eqP f g s w  = eql "orig" (f s) (g t) &&
               eql "mini" (f s) (g mini) &&
               eql "head" (f sa) (g ta) &&
               eql "tail" (f sb) (g tb)
    where t             = packS s
          mini          = packSChunkSize 10 s
          (sa,sb)       = splitAt m s
          (ta,tb)       = splitAtS m t
          l             = length s
          m | l == 0    = n
            | otherwise = n `mod` l
          n             = fromIntegral w
          eql d a b | a =^= b   = True
                    | otherwise = trace (d ++ ": " ++ show a ++ " /= " ++ show b) False

-- For tests that have O(n^2) running times or input sizes, resize
-- their inputs to the square root of the originals.
unsquare :: (Arbitrary a, Show a, Testable b) => (a -> b) -> Property
unsquare = forAll . sized $ \n -> resize (smaller n) arbitrary
    where smaller = round . (sqrt :: Double -> Double) . fromIntegral

s_Eq s            = (s==)    `eq` ((S.streamList s==) . S.streamList)
    where _types = s :: String
sf_Eq p s =
    ((L.filter p s==) . L.filter p) `eq`
    (((S.filter p $ S.streamList s)==) . S.filter p . S.streamList)
t_Eq s            = (s==)    `eq` ((T.pack s==) . T.pack)
tl_Eq s           = (s==)    `eq` ((TL.pack s==) . TL.pack)
s_Ord s           = (compare s) `eq` (compare (S.streamList s) . S.streamList)
    where _types = s :: String
sf_Ord p s =
    ((compare $ L.filter p s) . L.filter p) `eq`
    (compare (S.filter p $ S.streamList s) . S.filter p . S.streamList)
t_Ord s           = (compare s) `eq` (compare (T.pack s) . T.pack)
tl_Ord s          = (compare s) `eq` (compare (TL.pack s) . TL.pack)
t_Read            = id       `eq` (T.unpack . read . show)
tl_Read           = id       `eq` (TL.unpack . read . show)
t_Show            = show     `eq` (show . T.pack)
tl_Show           = show     `eq` (show . TL.pack)
t_mappend s       = mappend s`eqP` (unpackS . mappend (T.pack s))
tl_mappend s      = mappend s`eqP` (unpackS . mappend (TL.pack s))
t_mconcat         = unsquare (mconcat `eq` (unpackS . mconcat . L.map T.pack))
tl_mconcat        = unsquare (mconcat `eq` (unpackS . mconcat . L.map TL.pack))
t_IsString        = fromString  `eqP` (T.unpack . fromString)
tl_IsString       = fromString  `eqP` (TL.unpack . fromString)

s_cons x          = (x:)     `eqP` (unpackS . S.cons x)
sf_cons p x       = ((x:) . L.filter p) `eqP` (unpackS . S.cons x . S.filter p)
t_cons x          = (x:)     `eqP` (unpackS . T.cons x)
tl_cons x         = (x:)     `eqP` (unpackS . TL.cons x)
s_snoc x          = (++ [x]) `eqP` (unpackS . (flip S.snoc) x)
t_snoc x          = (++ [x]) `eqP` (unpackS . (flip T.snoc) x)
tl_snoc x         = (++ [x]) `eqP` (unpackS . (flip TL.snoc) x)
s_append s        = (s++)    `eqP` (unpackS . S.append (S.streamList s))
sf_append p s     = (L.filter p s++) `eqP` (unpackS . S.append (S.filter p $ S.streamList s))
t_append s        = (s++)    `eqP` (unpackS . T.append (packS s))

uncons (x:xs) = Just (x,xs)
uncons _      = Nothing

t_uncons s        = uncons   `eqP` (fmap (second unpackS) . T.uncons)
    where _types = s :: String
tl_uncons s       = uncons   `eqP` (fmap (second unpackS) . TL.uncons)
    where _types = s :: String
s_head            = head   `eqP` S.head
t_head            = head   `eqP` T.head
tl_head           = head   `eqP` TL.head
s_last            = last   `eqP` S.last
t_last            = last   `eqP` T.last
tl_last           = last   `eqP` TL.last
s_tail            = tail   `eqP` (unpackS . S.tail)
t_tail            = tail   `eqP` (unpackS . T.tail)
tl_tail           = tail   `eqP` (unpackS . TL.tail)
s_init            = init   `eqP` (unpackS . S.init)
t_init            = init   `eqP` (unpackS . T.init)
tl_init           = init   `eqP` (unpackS . TL.init)
s_null            = null   `eqP` S.null
t_null            = null   `eqP` T.null
tl_null           = null   `eqP` TL.null
s_length          = length `eqP` S.length
t_length          = length `eqP` T.length
tl_length         = length `eqP` (fromIntegral . TL.length)
t_map f           = map f  `eqP` (unpackS . T.map f)
tl_map f          = map f  `eqP` (unpackS . TL.map f)
t_intercalate c   = unsquare (L.intercalate c `eq` (unpackS . T.intercalate (packS c) . map packS))
tl_intercalate c  = unsquare (L.intercalate c `eq` (unpackS . TL.intercalate (TL.pack c) . map TL.pack))
t_intersperse c   = L.intersperse c `eqP` (unpackS . T.intersperse c)
tl_intersperse c  = L.intersperse c `eqP` (unpackS . TL.intersperse c)
t_transpose       = unsquare (L.transpose `eq` (map unpackS . T.transpose . map packS))
tl_transpose      = unsquare (L.transpose `eq` (map unpackS . TL.transpose . map TL.pack))
t_reverse         = L.reverse `eqP` (unpackS . T.reverse)
tl_reverse        = L.reverse `eqP` (unpackS . TL.reverse)
t_reverse_short n = L.reverse `eqP` (unpackS . S.reverse . shorten n . S.stream)

t_replace s d     = (L.intercalate d . split s) `eqP` (unpackS . T.replace (T.pack s) (T.pack d))

split :: (Eq a) => [a] -> [a] -> [[a]]
split pat src0
    | l == 0    = [src0]
    | otherwise = go src0
  where
    l           = length pat
    go src      = search 0 src
      where
        search _ [] = [src]
        search !n s@(_:s')
            | pat `L.isPrefixOf` s = take n src : go (drop l s)
            | otherwise            = search (n+1) s'

t_toCaseFold_length t = T.length (T.toCaseFold t) >= T.length t
t_toLower_length t = T.length (T.toLower t) >= T.length t
t_toLower_lower t = p (T.toLower t) >= p t
    where p = T.length . T.filter isLower
t_toUpper_length t = T.length (T.toUpper t) >= T.length t
t_toUpper_upper t = p (T.toUpper t) >= p t
    where p = T.length . T.filter isUpper

justifyLeft k c s = s ++ replicate (k - length s) c

s_justifyLeft k c = justifyLeft k c `eqP` (unpackS . S.justifyLeft k c)
t_justifyLeft k c = justifyLeft k c `eqP` (unpackS . T.justifyLeft k c)
t_justifyRight k c = jr k c `eqP` (unpackS . T.justifyRight k c)
    where jr m n s = replicate (m - length s) n ++ s

t_foldl f z       = L.foldl f z  `eqP` (T.foldl f z)
    where _types       = f :: Char -> Char -> Char
tl_foldl f z      = L.foldl f z  `eqP` (TL.foldl f z)
    where _types       = f :: Char -> Char -> Char
t_foldl' f z      = L.foldl' f z `eqP` T.foldl' f z
    where _types       = f :: Char -> Char -> Char
tl_foldl' f z     = L.foldl' f z `eqP` TL.foldl' f z
    where _types       = f :: Char -> Char -> Char
t_foldl1 f        = L.foldl1 f   `eqP` T.foldl1 f
tl_foldl1 f       = L.foldl1 f   `eqP` TL.foldl1 f
t_foldl1' f       = L.foldl1' f  `eqP` T.foldl1' f
tl_foldl1' f      = L.foldl1' f  `eqP` TL.foldl1' f
t_foldr f z       = L.foldr f z  `eqP` T.foldr f z
    where _types       = f :: Char -> Char -> Char
tl_foldr f z      = L.foldr f z  `eqP` TL.foldr f z
    where _types       = f :: Char -> Char -> Char
t_foldr1 f        = L.foldr1 f   `eqP` T.foldr1 f
tl_foldr1 f       = L.foldr1 f   `eqP` TL.foldr1 f

t_concat          = unsquare (L.concat `eq` (unpackS . T.concat . map packS))
tl_concat         = unsquare (L.concat `eq` (unpackS . TL.concat . map TL.pack))
t_concatMap f     = unsquare (L.concatMap f `eqP` (unpackS . T.concatMap (packS . f)))
tl_concatMap f    = unsquare (L.concatMap f `eqP` (unpackS . TL.concatMap (TL.pack . f)))
t_any p           = L.any p       `eqP` T.any p
tl_any p          = L.any p       `eqP` TL.any p
t_all p           = L.all p       `eqP` T.all p
tl_all p          = L.all p       `eqP` TL.all p
t_maximum         = L.maximum     `eqP` T.maximum
tl_maximum        = L.maximum     `eqP` TL.maximum
t_minimum         = L.minimum     `eqP` T.minimum
tl_minimum        = L.minimum     `eqP` TL.minimum

t_scanl f z       = L.scanl f z   `eqP` (unpackS . T.scanl f z)
tl_scanl f z      = L.scanl f z   `eqP` (unpackS . TL.scanl f z)
t_scanl1 f        = L.scanl1 f    `eqP` (unpackS . T.scanl1 f)
tl_scanl1 f       = L.scanl1 f    `eqP` (unpackS . TL.scanl1 f)
t_scanr f z       = L.scanr f z   `eqP` (unpackS . T.scanr f z)
tl_scanr f z      = L.scanr f z   `eqP` (unpackS . TL.scanr f z)
t_scanr1 f        = L.scanr1 f    `eqP` (unpackS . T.scanr1 f)
tl_scanr1 f       = L.scanr1 f    `eqP` (unpackS . TL.scanr1 f)

t_mapAccumL f z   = unsquare (L.mapAccumL f z `eqP` (second unpackS . T.mapAccumL f z))
    where _types = f :: Int -> Char -> (Int,Char)
tl_mapAccumL f z  = unsquare (L.mapAccumL f z `eqP` (second unpackS . TL.mapAccumL f z))
    where _types = f :: Int -> Char -> (Int,Char)
t_mapAccumR f z   = unsquare (L.mapAccumR f z `eqP` (second unpackS . T.mapAccumR f z))
    where _types = f :: Int -> Char -> (Int,Char)
tl_mapAccumR f z   = unsquare (L.mapAccumR f z `eqP` (second unpackS . TL.mapAccumR f z))
    where _types = f :: Int -> Char -> (Int,Char)

t_replicate n     = L.replicate n `eq` (unpackS . T.replicate n)
tl_replicate n    = L.replicate n `eq` (unpackS . TL.replicate n)

unf :: Int -> Char -> Maybe (Char, Char)
unf n c | fromEnum c * 100 > n = Nothing
        | otherwise            = Just (c, succ c)

t_unfoldr n       = L.unfoldr (unf n) `eq` (unpackS . T.unfoldr (unf n))
tl_unfoldr n      = L.unfoldr (unf n) `eq` (unpackS . TL.unfoldr (unf n))
t_unfoldrN n m    = (L.take n . L.unfoldr (unf m)) `eq`
                         (unpackS . T.unfoldrN n (unf m))
tl_unfoldrN n m   = (L.take n . L.unfoldr (unf m)) `eq`
                         (unpackS . TL.unfoldrN (fromIntegral n) (unf m))

unpack2 :: (Stringy s) => (s,s) -> (String,String)
unpack2 = unpackS *** unpackS

s_take n          = L.take n      `eqP` (unpackS . S.take n)
t_take n          = L.take n      `eqP` (unpackS . T.take n)
tl_take n         = L.take n      `eqP` (unpackS . TL.take (fromIntegral n))
s_drop n          = L.drop n      `eqP` (unpackS . S.drop n)
t_drop n          = L.drop n      `eqP` (unpackS . T.drop n)
tl_drop n         = L.drop n      `eqP` (unpackS . TL.drop n)
s_takeWhile p     = L.takeWhile p `eqP` (unpackS . S.takeWhile p)
t_takeWhile p     = L.takeWhile p `eqP` (unpackS . T.takeWhile p)
tl_takeWhile p    = L.takeWhile p `eqP` (unpackS . TL.takeWhile p)
s_dropWhile p     = L.dropWhile p `eqP` (unpackS . S.dropWhile p)
t_dropWhile p     = L.dropWhile p `eqP` (unpackS . T.dropWhile p)
tl_dropWhile p    = L.dropWhile p `eqP` (unpackS . S.dropWhile p)
s_dropWhileEnd p  = T.dropWhileEnd p `eq` (S.reverse . S.dropWhile p . S.reverseStream)
t_dropWhileEnd p  = (T.reverse . T.dropWhile p . T.reverse) `eq` T.dropWhileEnd p
t_dropAround p    = (T.dropWhile p . T.dropWhileEnd p) `eq` T.dropAround p
t_stripStart       = T.dropWhile isSpace `eq` T.stripStart
t_stripEnd      = T.dropWhileEnd isSpace `eq` T.stripEnd
t_strip           = T.dropAround isSpace `eq` T.strip
t_splitAt n       = L.splitAt n   `eqP` (unpack2 . T.splitAt n)
tl_splitAt n      = L.splitAt n   `eqP` (unpack2 . TL.splitAt (fromIntegral n))
t_span p          = L.span p      `eqP` (unpack2 . T.span p)
tl_span p         = L.span p      `eqP` (unpack2 . TL.span p)
t_break p         = L.break p     `eqP` (unpack2 . T.break p)
tl_break p        = L.break p     `eqP` (unpack2 . TL.break p)
t_group           = L.group       `eqP` (map unpackS . T.group)
tl_group          = L.group       `eqP` (map unpackS . TL.group)
t_groupBy p       = L.groupBy p   `eqP` (map unpackS . T.groupBy p)
tl_groupBy p      = L.groupBy p   `eqP` (map unpackS . TL.groupBy p)
t_inits           = L.inits       `eqP` (map unpackS . T.inits)
tl_inits          = L.inits       `eqP` (map unpackS . TL.inits)
t_tails           = L.tails       `eqP` (map unpackS . T.tails)
tl_tails          = L.tails       `eqP` (map unpackS . TL.tails)

t_split_i t       = id `eq` (T.intercalate t . T.split t)
t_splitTimes_i k t = id `eq` (T.intercalate t . T.splitTimes k t)
t_splitTimes_split k t = T.splitTimes k t `eq` \u ->
                              case L.splitAt k (T.split t u) of
                                (a,[]) -> a
                                (a,b)  -> a ++ [T.intercalate t b]
t_splitTimesEnd_i k t = id `eq` (T.intercalate t . T.splitTimesEnd k t)
t_splitTimesEnd_split t = T.splitTimesEnd maxBound t `eq` T.split t
tl_split_i c      = id `eq` (TL.intercalate (TL.singleton c) . TL.split c)

t_splitWith p     = splitWith p `eqP` (map unpackS . T.splitWith p)
t_splitWith_count c = (L.length . T.splitWith (==c)) `eq` ((1+) . T.count c)
t_splitWith_split c = T.splitWith (==c) `eq` T.split (T.singleton c)
t_splitWith_Csplit c = T.splitWith (==c) `eq` C.split c
tl_splitWith p    = splitWith p `eqP` (map unpackS . TL.splitWith p)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] =  [[]]
splitWith p xs = loop xs
    where loop s | null s'   = [l]
                 | otherwise = l : loop (tail s')
              where (l, s') = break p s

t_chunksOf_same_lengths k = all ((==k) . T.length) . ini . T.chunksOf k
  where ini [] = []
        ini xs = init xs

t_chunksOf_length k t = len == T.length t || (k <= 0 && len == 0)
  where len = L.sum . L.map T.length $ T.chunksOf k t

t_breakSubstring_isInfixOf s l
                     = T.isInfixOf s l ==
                       T.null s || (not . T.null . snd $ C.breakSubstring s l)
t_breakSubstringC c
                     = L.break (==c) `eqP`
                       (unpack2 . C.breakSubstring (T.singleton c))

t_lines           = L.lines       `eqP` (map unpackS . T.lines)
tl_lines          = L.lines       `eqP` (map unpackS . TL.lines)
{-
t_lines'          = lines'        `eqP` (map unpackS . T.lines')
    where lines' "" =  []
          lines' s =  let (l, s') = break eol s
                      in  l : case s' of
                                []      -> []
                                ('\r':'\n':s'') -> lines' s''
                                (_:s'') -> lines' s''
          eol c = c == '\r' || c == '\n'
-}
t_words           = L.words       `eqP` (map unpackS . T.words)

tl_words          = L.words       `eqP` (map unpackS . TL.words)
t_unlines         = unsquare (L.unlines `eq` (unpackS . T.unlines . map packS))
tl_unlines        = unsquare (L.unlines `eq` (unpackS . TL.unlines . map packS))
t_unwords         = unsquare (L.unwords `eq` (unpackS . T.unwords . map packS))
tl_unwords        = unsquare (L.unwords `eq` (unpackS . TL.unwords . map packS))

s_isPrefixOf s    = L.isPrefixOf s`eqP` (S.isPrefixOf (S.stream $ packS s) . S.stream)
t_isPrefixOf s    = L.isPrefixOf s`eqP` T.isPrefixOf (packS s)
tl_isPrefixOf s   = L.isPrefixOf s`eqP` TL.isPrefixOf (packS s)
t_isSuffixOf s    = L.isSuffixOf s`eqP` T.isSuffixOf (packS s)
tl_isSuffixOf s   = L.isSuffixOf s`eqP` TL.isSuffixOf (packS s)
t_isInfixOf s     = L.isInfixOf s `eqP` T.isInfixOf (packS s)
tl_isInfixOf s    = L.isInfixOf s `eqP` TL.isInfixOf (packS s)

t_elem c          = L.elem c      `eqP` T.elem c
tl_elem c         = L.elem c      `eqP` TL.elem c
t_filter p        = L.filter p    `eqP` (unpackS . T.filter p)
tl_filter p       = L.filter p    `eqP` (unpackS . TL.filter p)
t_find p          = L.find p      `eqP` T.find p
tl_find p         = L.find p      `eqP` TL.find p
t_partition p     = L.partition p `eqP` (unpack2 . T.partition p)
tl_partition p    = L.partition p `eqP` (unpack2 . TL.partition p)

t_index s         = forAll (choose (-l,l*2)) ((s L.!!) `eq` T.index (packS s))
    where l = L.length s

tl_index s        = forAll (choose (-l,l*2)) ((s L.!!) `eq` (TL.index (packS s) . fromIntegral))
    where l = L.length s

t_findIndex p     = L.findIndex p `eqP` T.findIndex p
tl_findIndex p    = (fmap fromIntegral . L.findIndex p) `eqP` TL.findIndex p
t_findIndices p   = L.findIndices p `eqP` T.findIndices p
tl_findIndices p  = (fmap fromIntegral . L.findIndices p) `eqP` TL.findIndices p
t_elemIndex c     = L.elemIndex c `eqP` T.elemIndex c
tl_elemIndex c    = (fmap fromIntegral . L.elemIndex c) `eqP` TL.elemIndex c
t_elemIndices c   = L.elemIndices c`eqP` T.elemIndices c
tl_elemIndices c  = (fmap fromIntegral . L.elemIndices c) `eqP` TL.elemIndices c
t_count c         = (L.length . L.elemIndices c) `eqP` T.count c
tl_count c        = (fromIntegral . L.length . L.elemIndices c) `eqP` TL.count c
t_zip s           = L.zip s `eqP` T.zip (packS s)
tl_zip s          = L.zip s `eqP` TL.zip (packS s)
t_zipWith c s     = L.zipWith c s `eqP` (unpackS . T.zipWith c (packS s))
tl_zipWith c s    = L.zipWith c s `eqP` (unpackS . TL.zipWith c (packS s))

-- Regression tests.
s_filter_eq s = S.filter p t == S.streamList (filter p s)
    where p = (/= S.last t)
          t = S.streamList s

-- Make a stream appear shorter than it really is, to ensure that
-- functions that consume inaccurately sized streams behave
-- themselves.
shorten :: Int -> S.Stream a -> S.Stream a
shorten n t@(S.Stream arr off len)
    | n < len && n > 0 = S.Stream arr off n
    | otherwise        = t

main = defaultMain tests

tests = [
  testGroup "creation/elimination" [
    testProperty "t_pack_unpack" t_pack_unpack,
    testProperty "tl_pack_unpack" tl_pack_unpack,
    testProperty "t_stream_unstream" t_stream_unstream,
    testProperty "tl_stream_unstream" tl_stream_unstream,
    testProperty "t_reverse_stream" t_reverse_stream,
    testProperty "t_singleton" t_singleton,
    testProperty "tl_singleton" tl_singleton,
    testProperty "tl_unstreamChunks" tl_unstreamChunks,
    testProperty "tl_chunk_unchunk" tl_chunk_unchunk
  ],

  testGroup "transcoding" [
    testProperty "t_ascii" t_ascii,
    testProperty "t_utf8" t_utf8,
    testProperty "tl_utf8" tl_utf8,
    testProperty "t_utf16LE" t_utf16LE,
    testProperty "t_utf16BE" t_utf16BE,
    testProperty "t_utf32LE" t_utf32LE,
    testProperty "t_utf32BE" t_utf32BE
  ],

  testGroup "instances" [
    testProperty "s_Eq" s_Eq,
    testProperty "sf_Eq" sf_Eq,
    testProperty "t_Eq" t_Eq,
    testProperty "tl_Eq" tl_Eq,
    testProperty "s_Ord" s_Ord,
    testProperty "sf_Ord" sf_Ord,
    testProperty "t_Ord" t_Ord,
    testProperty "tl_Ord" tl_Ord,
    testProperty "t_Read" t_Read,
    testProperty "tl_Read" tl_Read,
    testProperty "t_Show" t_Show,
    testProperty "tl_Show" tl_Show,
    testProperty "t_mappend" t_mappend,
    testProperty "tl_mappend" tl_mappend,
    testProperty "t_mconcat" t_mconcat,
    testProperty "tl_mconcat" tl_mconcat,
    testProperty "t_IsString" t_IsString,
    testProperty "tl_IsString" tl_IsString
  ],

  testGroup "basics" [
    testProperty "s_cons" s_cons,
    testProperty "sf_cons" sf_cons,
    testProperty "t_cons" t_cons,
    testProperty "tl_cons" tl_cons,
    testProperty "s_snoc" s_snoc,
    testProperty "t_snoc" t_snoc,
    testProperty "tl_snoc" tl_snoc,
    testProperty "s_append" s_append,
    testProperty "sf_append" sf_append,
    testProperty "t_append" t_append,
    testProperty "t_uncons" t_uncons,
    testProperty "tl_uncons" tl_uncons,
    testProperty "s_head" s_head,
    testProperty "t_head" t_head,
    testProperty "tl_head" tl_head,
    testProperty "s_last" s_last,
    testProperty "t_last" t_last,
    testProperty "tl_last" tl_last,
    testProperty "s_tail" s_tail,
    testProperty "t_tail" t_tail,
    testProperty "tl_tail" tl_tail,
    testProperty "s_init" s_init,
    testProperty "t_init" t_init,
    testProperty "tl_init" tl_init,
    testProperty "s_null" s_null,
    testProperty "t_null" t_null,
    testProperty "tl_null" tl_null,
    testProperty "s_length" s_length,
    testProperty "t_length" t_length,
    testProperty "tl_length" tl_length
  ],

  testGroup "transformations" [
    testProperty "t_map" t_map,
    testProperty "tl_map" tl_map,
    testProperty "t_intercalate" t_intercalate,
    testProperty "tl_intercalate" tl_intercalate,
    testProperty "t_intersperse" t_intersperse,
    testProperty "tl_intersperse" tl_intersperse,
    testProperty "t_transpose" t_transpose,
    testProperty "tl_transpose" tl_transpose,
    testProperty "t_reverse" t_reverse,
    testProperty "tl_reverse" tl_reverse,
    testProperty "t_reverse_short" t_reverse_short,
    testProperty "t_replace" t_replace,

    testGroup "case conversion" [
      testProperty "t_toCaseFold_length" t_toCaseFold_length,
      testProperty "t_toLower_length" t_toLower_length,
      testProperty "t_toLower_lower" t_toLower_lower,
      testProperty "t_toUpper_length" t_toUpper_length,
      testProperty "t_toUpper_upper" t_toUpper_upper
    ],

    testGroup "justification" [
      testProperty "s_justifyLeft" s_justifyLeft,
      testProperty "t_justifyLeft" t_justifyLeft,
      testProperty "t_justifyRight" t_justifyRight
    ]
  ],

  testGroup "folds" [
    testProperty "t_foldl" t_foldl,
    testProperty "tl_foldl" tl_foldl,
    testProperty "t_foldl'" t_foldl',
    testProperty "tl_foldl'" tl_foldl',
    testProperty "t_foldl1" t_foldl1,
    testProperty "tl_foldl1" tl_foldl1,
    testProperty "t_foldl1'" t_foldl1',
    testProperty "tl_foldl1'" tl_foldl1',
    testProperty "t_foldr" t_foldr,
    testProperty "tl_foldr" tl_foldr,
    testProperty "t_foldr1" t_foldr1,
    testProperty "tl_foldr1" tl_foldr1,

    testGroup "special" [
      testProperty "t_concat" t_concat,
      testProperty "tl_concat" tl_concat,
      testProperty "t_concatMap" t_concatMap,
      testProperty "tl_concatMap" tl_concatMap,
      testProperty "t_any" t_any,
      testProperty "tl_any" tl_any,
      testProperty "t_all" t_all,
      testProperty "tl_all" tl_all,
      testProperty "t_maximum" t_maximum,
      testProperty "tl_maximum" tl_maximum,
      testProperty "t_minimum" t_minimum,
      testProperty "tl_minimum" tl_minimum
    ]
  ],

  testGroup "construction" [
    testGroup "scans" [
      testProperty "t_scanl" t_scanl,
      testProperty "tl_scanl" tl_scanl,
      testProperty "t_scanl1" t_scanl1,
      testProperty "tl_scanl1" tl_scanl1,
      testProperty "t_scanr" t_scanr,
      testProperty "tl_scanr" tl_scanr,
      testProperty "t_scanr1" t_scanr1,
      testProperty "tl_scanr1" tl_scanr1
    ],

    testGroup "mapAccum" [
      testProperty "t_mapAccumL" t_mapAccumL,
      testProperty "tl_mapAccumL" tl_mapAccumL,
      testProperty "t_mapAccumR" t_mapAccumR,
      testProperty "tl_mapAccumR" tl_mapAccumR
    ],

    testGroup "unfolds" [
      testProperty "t_replicate" t_replicate,
      testProperty "tl_replicate" tl_replicate,
      testProperty "t_unfoldr" t_unfoldr,
      testProperty "tl_unfoldr" tl_unfoldr,
      testProperty "t_unfoldrN" t_unfoldrN,
      testProperty "tl_unfoldrN" tl_unfoldrN
    ]
  ],

  testGroup "substrings" [
    testGroup "breaking" [
      testProperty "s_take" s_take,
      testProperty "t_take" t_take,
      testProperty "tl_take" tl_take,
      testProperty "s_drop" s_drop,
      testProperty "t_drop" t_drop,
      testProperty "tl_drop" tl_drop,
      testProperty "s_takeWhile" s_takeWhile,
      testProperty "t_takeWhile" t_takeWhile,
      testProperty "tl_takeWhile" tl_takeWhile,
      testProperty "s_dropWhile" s_dropWhile,
      testProperty "t_dropWhile" t_dropWhile,
      testProperty "tl_dropWhile" tl_dropWhile,
      testProperty "s_dropWhileEnd" s_dropWhileEnd,
      testProperty "t_dropWhileEnd" t_dropWhileEnd,
      testProperty "t_dropAround" t_dropAround,
      testProperty "t_stripStart" t_stripStart,
      testProperty "t_stripEnd" t_stripEnd,
      testProperty "t_strip" t_strip,
      testProperty "t_splitAt" t_splitAt,
      testProperty "tl_splitAt" tl_splitAt,
      testProperty "t_span" t_span,
      testProperty "tl_span" tl_span,
      testProperty "t_break" t_break,
      testProperty "tl_break" tl_break,
      testProperty "t_group" t_group,
      testProperty "tl_group" tl_group,
      testProperty "t_groupBy" t_groupBy,
      testProperty "tl_groupBy" tl_groupBy,
      testProperty "t_inits" t_inits,
      testProperty "tl_inits" tl_inits,
      testProperty "t_tails" t_tails,
      testProperty "tl_tails" tl_tails
    ],

    testGroup "breaking many" [
      testProperty "t_split_i" t_split_i,
      testProperty "t_splitTimes_i" t_splitTimes_i,
      testProperty "t_splitTimes_split" t_splitTimes_split,
      testProperty "t_splitTimesEnd_i" t_splitTimesEnd_i,
      testProperty "t_splitTimesEnd_split" t_splitTimesEnd_split,
      testProperty "tl_split_i" tl_split_i,
      testProperty "t_splitWith" t_splitWith,
      testProperty "t_splitWith_count" t_splitWith_count,
      testProperty "t_splitWith_split" t_splitWith_split,
      testProperty "t_splitWith_Csplit" t_splitWith_Csplit,
      testProperty "tl_splitWith" tl_splitWith,
      testProperty "t_chunksOf_same_lengths" t_chunksOf_same_lengths,
      testProperty "t_chunksOf_length" t_chunksOf_length,
      testProperty "t_breakSubstringC" t_breakSubstringC,
      testProperty "t_breakSubstring_isInfixOf" t_breakSubstring_isInfixOf
    ],

    testGroup "lines and words" [
      testProperty "t_lines" t_lines,
      testProperty "tl_lines" tl_lines,
    --testProperty "t_lines'" t_lines',
      testProperty "t_words" t_words,
      testProperty "tl_words" tl_words,
      testProperty "t_unlines" t_unlines,
      testProperty "tl_unlines" tl_unlines,
      testProperty "t_unwords" t_unwords,
      testProperty "tl_unwords" tl_unwords
    ]
  ],

  testGroup "predicates" [
    testProperty "s_isPrefixOf" s_isPrefixOf,
    testProperty "t_isPrefixOf" t_isPrefixOf,
    testProperty "tl_isPrefixOf" tl_isPrefixOf,
    testProperty "t_isSuffixOf" t_isSuffixOf,
    testProperty "tl_isSuffixOf" tl_isSuffixOf,
    testProperty "t_isInfixOf" t_isInfixOf,
    testProperty "tl_isInfixOf" tl_isInfixOf
  ],

  testGroup "searching" [
    testProperty "t_elem" t_elem,
    testProperty "tl_elem" tl_elem,
    testProperty "t_filter" t_filter,
    testProperty "tl_filter" tl_filter,
    testProperty "t_find" t_find,
    testProperty "tl_find" tl_find,
    testProperty "t_partition" t_partition,
    testProperty "tl_partition" tl_partition
  ],

  testGroup "indexing" [
    testProperty "t_index" t_index,
    testProperty "tl_index" tl_index,
    testProperty "t_findIndex" t_findIndex,
    testProperty "tl_findIndex" tl_findIndex,
    testProperty "t_findIndices" t_findIndices,
    testProperty "tl_findIndices" tl_findIndices,
    testProperty "t_elemIndex" t_elemIndex,
    testProperty "tl_elemIndex" tl_elemIndex,
    testProperty "t_elemIndices" t_elemIndices,
    testProperty "tl_elemIndices" tl_elemIndices,
    testProperty "t_count" t_count,
    testProperty "tl_count" tl_count
  ],

  testGroup "zips" [
    testProperty "t_zip" t_zip,
    testProperty "tl_zip" tl_zip,
    testProperty "t_zipWith" t_zipWith,
    testProperty "tl_zipWith" tl_zipWith
  ],

  testGroup "regressions" [
    testProperty "s_filter_eq" s_filter_eq
  ]
 ]
