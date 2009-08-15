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
    where a              = T.map (\c -> chr (ord c `mod` 128)) t
prop_T_utf8              = (E.decodeUtf8 . E.encodeUtf8) `eq` id
prop_TL_utf8             = (EL.decodeUtf8 . EL.encodeUtf8) `eq` id
prop_T_utf16LE           = (E.decodeUtf16LE . E.encodeUtf16LE) `eq` id
prop_T_utf16BE           = (E.decodeUtf16BE . E.encodeUtf16BE) `eq` id
prop_T_utf32LE           = (E.decodeUtf32LE . E.encodeUtf32LE) `eq` id
prop_T_utf32BE           = (E.decodeUtf32BE . E.encodeUtf32BE) `eq` id

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

prop_S_Eq s            = (s==)    `eq` ((S.streamList s==) . S.streamList)
    where _types = s :: String
prop_Sf_Eq p s =
    ((L.filter p s==) . L.filter p) `eq`
    (((S.filter p $ S.streamList s)==) . S.filter p . S.streamList)
prop_T_Eq s            = (s==)    `eq` ((T.pack s==) . T.pack)
prop_TL_Eq s           = (s==)    `eq` ((TL.pack s==) . TL.pack)
prop_S_Ord s           = (compare s) `eq` (compare (S.streamList s) . S.streamList)
    where _types = s :: String
prop_Sf_Ord p s =
    ((compare $ L.filter p s) . L.filter p) `eq`
    (compare (S.filter p $ S.streamList s) . S.filter p . S.streamList)
prop_T_Ord s           = (compare s) `eq` (compare (T.pack s) . T.pack)
prop_TL_Ord s          = (compare s) `eq` (compare (TL.pack s) . TL.pack)
prop_T_Read            = id       `eq` (T.unpack . read . show)
prop_TL_Read           = id       `eq` (TL.unpack . read . show)
prop_T_Show            = show     `eq` (show . T.pack)
prop_TL_Show           = show     `eq` (show . TL.pack)
prop_T_mappend s       = mappend s`eqP` (unpackS . mappend (T.pack s))
prop_TL_mappend s      = mappend s`eqP` (unpackS . mappend (TL.pack s))
prop_T_mconcat         = unsquare (mconcat `eq` (unpackS . mconcat . L.map T.pack))
prop_TL_mconcat        = unsquare (mconcat `eq` (unpackS . mconcat . L.map TL.pack))
prop_T_IsString        = fromString  `eqP` (T.unpack . fromString)
prop_TL_IsString       = fromString  `eqP` (TL.unpack . fromString)

prop_S_cons x          = (x:)     `eqP` (unpackS . S.cons x)
prop_Sf_cons p x       = ((x:) . L.filter p) `eqP` (unpackS . S.cons x . S.filter p)
prop_T_cons x          = (x:)     `eqP` (unpackS . T.cons x)
prop_TL_cons x         = (x:)     `eqP` (unpackS . TL.cons x)
prop_S_snoc x          = (++ [x]) `eqP` (unpackS . (flip S.snoc) x)
prop_T_snoc x          = (++ [x]) `eqP` (unpackS . (flip T.snoc) x)
prop_TL_snoc x         = (++ [x]) `eqP` (unpackS . (flip TL.snoc) x)
prop_S_append s        = (s++)    `eqP` (unpackS . S.append (S.streamList s))
prop_Sf_append p s     = (L.filter p s++) `eqP` (unpackS . S.append (S.filter p $ S.streamList s))
prop_T_append s        = (s++)    `eqP` (unpackS . T.append (packS s))

uncons (x:xs) = Just (x,xs)
uncons _      = Nothing

prop_T_uncons s        = uncons   `eqP` (fmap (second unpackS) . T.uncons)
    where _types = s :: String
prop_TL_uncons s       = uncons   `eqP` (fmap (second unpackS) . TL.uncons)
    where _types = s :: String
prop_S_head            = head   `eqP` S.head
prop_T_head            = head   `eqP` T.head
prop_TL_head           = head   `eqP` TL.head
prop_S_last            = last   `eqP` S.last
prop_T_last            = last   `eqP` T.last
prop_TL_last           = last   `eqP` TL.last
prop_S_tail            = tail   `eqP` (unpackS . S.tail)
prop_T_tail            = tail   `eqP` (unpackS . T.tail)
prop_TL_tail           = tail   `eqP` (unpackS . TL.tail)
prop_S_init            = init   `eqP` (unpackS . S.init)
prop_T_init            = init   `eqP` (unpackS . T.init)
prop_TL_init           = init   `eqP` (unpackS . TL.init)
prop_S_null            = null   `eqP` S.null
prop_T_null            = null   `eqP` T.null
prop_TL_null           = null   `eqP` TL.null
prop_S_length          = length `eqP` S.length
prop_T_length          = length `eqP` T.length
prop_TL_length         = length `eqP` (fromIntegral . TL.length)
prop_T_map f           = map f  `eqP` (unpackS . T.map f)
prop_TL_map f          = map f  `eqP` (unpackS . TL.map f)
prop_T_intercalate c   = unsquare (L.intercalate c `eq` (unpackS . T.intercalate (packS c) . map packS))
prop_TL_intercalate c  = unsquare (L.intercalate c `eq` (unpackS . TL.intercalate (TL.pack c) . map TL.pack))
prop_T_intersperse c   = L.intersperse c `eqP` (unpackS . T.intersperse c)
prop_TL_intersperse c  = L.intersperse c `eqP` (unpackS . TL.intersperse c)
prop_T_transpose       = unsquare (L.transpose `eq` (map unpackS . T.transpose . map packS))
prop_TL_transpose      = unsquare (L.transpose `eq` (map unpackS . TL.transpose . map TL.pack))
prop_T_reverse         = L.reverse `eqP` (unpackS . T.reverse)
prop_TL_reverse        = L.reverse `eqP` (unpackS . TL.reverse)
prop_T_reverse_short n = L.reverse `eqP` (unpackS . S.reverse . shorten n . S.stream)

prop_T_replace s d     = (L.intercalate d . split s) `eqP` (unpackS . T.replace (T.pack s) (T.pack d))

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

prop_T_toCaseFold_length t = T.length (T.toCaseFold t) >= T.length t
prop_T_toLower_length t = T.length (T.toLower t) >= T.length t
prop_T_toLower_lower t = p (T.toLower t) >= p t
    where p = T.length . T.filter isLower
prop_T_toUpper_length t = T.length (T.toUpper t) >= T.length t
prop_T_toUpper_upper t = p (T.toUpper t) >= p t
    where p = T.length . T.filter isUpper

justifyLeft k c s = s ++ replicate (k - length s) c

prop_S_justifyLeft k c = justifyLeft k c `eqP` (unpackS . S.justifyLeft k c)
prop_T_justifyLeft k c = justifyLeft k c `eqP` (unpackS . T.justifyLeft k c)
prop_T_justifyRight k c = jr k c `eqP` (unpackS . T.justifyRight k c)
    where jr m n s = replicate (m - length s) n ++ s

prop_T_foldl f z       = L.foldl f z  `eqP` (T.foldl f z)
    where _types       = f :: Char -> Char -> Char
prop_TL_foldl f z      = L.foldl f z  `eqP` (TL.foldl f z)
    where _types       = f :: Char -> Char -> Char
prop_T_foldl' f z      = L.foldl' f z `eqP` T.foldl' f z
    where _types       = f :: Char -> Char -> Char
prop_TL_foldl' f z     = L.foldl' f z `eqP` TL.foldl' f z
    where _types       = f :: Char -> Char -> Char
prop_T_foldl1 f        = L.foldl1 f   `eqP` T.foldl1 f
prop_TL_foldl1 f       = L.foldl1 f   `eqP` TL.foldl1 f
prop_T_foldl1' f       = L.foldl1' f  `eqP` T.foldl1' f
prop_TL_foldl1' f      = L.foldl1' f  `eqP` TL.foldl1' f
prop_T_foldr f z       = L.foldr f z  `eqP` T.foldr f z
    where _types       = f :: Char -> Char -> Char
prop_TL_foldr f z      = L.foldr f z  `eqP` TL.foldr f z
    where _types       = f :: Char -> Char -> Char
prop_T_foldr1 f        = L.foldr1 f   `eqP` T.foldr1 f
prop_TL_foldr1 f       = L.foldr1 f   `eqP` TL.foldr1 f

prop_T_concat          = unsquare (L.concat `eq` (unpackS . T.concat . map packS))
prop_TL_concat         = unsquare (L.concat `eq` (unpackS . TL.concat . map TL.pack))
prop_T_concatMap f     = unsquare (L.concatMap f `eqP` (unpackS . T.concatMap (packS . f)))
prop_TL_concatMap f    = unsquare (L.concatMap f `eqP` (unpackS . TL.concatMap (TL.pack . f)))
prop_T_any p           = L.any p       `eqP` T.any p
prop_TL_any p          = L.any p       `eqP` TL.any p
prop_T_all p           = L.all p       `eqP` T.all p
prop_TL_all p          = L.all p       `eqP` TL.all p
prop_T_maximum         = L.maximum     `eqP` T.maximum
prop_TL_maximum        = L.maximum     `eqP` TL.maximum
prop_T_minimum         = L.minimum     `eqP` T.minimum
prop_TL_minimum        = L.minimum     `eqP` TL.minimum

prop_T_scanl f z       = L.scanl f z   `eqP` (unpackS . T.scanl f z)
prop_TL_scanl f z      = L.scanl f z   `eqP` (unpackS . TL.scanl f z)
prop_T_scanl1 f        = L.scanl1 f    `eqP` (unpackS . T.scanl1 f)
prop_TL_scanl1 f       = L.scanl1 f    `eqP` (unpackS . TL.scanl1 f)
prop_T_scanr f z       = L.scanr f z   `eqP` (unpackS . T.scanr f z)
prop_TL_scanr f z      = L.scanr f z   `eqP` (unpackS . TL.scanr f z)
prop_T_scanr1 f        = L.scanr1 f    `eqP` (unpackS . T.scanr1 f)
prop_TL_scanr1 f       = L.scanr1 f    `eqP` (unpackS . TL.scanr1 f)

prop_T_mapAccumL f z   = unsquare (L.mapAccumL f z `eqP` (second unpackS . T.mapAccumL f z))
    where _types = f :: Int -> Char -> (Int,Char)
prop_TL_mapAccumL f z  = unsquare (L.mapAccumL f z `eqP` (second unpackS . TL.mapAccumL f z))
    where _types = f :: Int -> Char -> (Int,Char)
prop_T_mapAccumR f z   = unsquare (L.mapAccumR f z `eqP` (second unpackS . T.mapAccumR f z))
    where _types = f :: Int -> Char -> (Int,Char)
prop_TL_mapAccumR f z   = unsquare (L.mapAccumR f z `eqP` (second unpackS . TL.mapAccumR f z))
    where _types = f :: Int -> Char -> (Int,Char)

prop_T_replicate n     = L.replicate n `eq` (unpackS . T.replicate n)
prop_TL_replicate n    = L.replicate n `eq` (unpackS . TL.replicate n)

unf :: Int -> Char -> Maybe (Char, Char)
unf n c | fromEnum c * 100 > n = Nothing
        | otherwise            = Just (c, succ c)

prop_T_unfoldr n       = L.unfoldr (unf n) `eq` (unpackS . T.unfoldr (unf n))
prop_TL_unfoldr n      = L.unfoldr (unf n) `eq` (unpackS . TL.unfoldr (unf n))
prop_T_unfoldrN n m    = (L.take n . L.unfoldr (unf m)) `eq`
                         (unpackS . T.unfoldrN n (unf m))
prop_TL_unfoldrN n m   = (L.take n . L.unfoldr (unf m)) `eq`
                         (unpackS . TL.unfoldrN (fromIntegral n) (unf m))

unpack2 :: (Stringy s) => (s,s) -> (String,String)
unpack2 = unpackS *** unpackS

prop_S_take n          = L.take n      `eqP` (unpackS . S.take n)
prop_T_take n          = L.take n      `eqP` (unpackS . T.take n)
prop_TL_take n         = L.take n      `eqP` (unpackS . TL.take (fromIntegral n))
prop_S_drop n          = L.drop n      `eqP` (unpackS . S.drop n)
prop_T_drop n          = L.drop n      `eqP` (unpackS . T.drop n)
prop_TL_drop n         = L.drop n      `eqP` (unpackS . TL.drop n)
prop_S_takeWhile p     = L.takeWhile p `eqP` (unpackS . S.takeWhile p)
prop_T_takeWhile p     = L.takeWhile p `eqP` (unpackS . T.takeWhile p)
prop_TL_takeWhile p    = L.takeWhile p `eqP` (unpackS . TL.takeWhile p)
prop_S_dropWhile p     = L.dropWhile p `eqP` (unpackS . S.dropWhile p)
prop_T_dropWhile p     = L.dropWhile p `eqP` (unpackS . T.dropWhile p)
prop_TL_dropWhile p    = L.dropWhile p `eqP` (unpackS . S.dropWhile p)
prop_S_dropWhileEnd p  = T.dropWhileEnd p `eq` (S.reverse . S.dropWhile p . S.reverseStream)
prop_T_dropWhileEnd p  = (T.reverse . T.dropWhile p . T.reverse) `eq` T.dropWhileEnd p
prop_T_dropAround p    = (T.dropWhile p . T.dropWhileEnd p) `eq` T.dropAround p
prop_T_stripStart       = T.dropWhile isSpace `eq` T.stripStart
prop_T_stripEnd      = T.dropWhileEnd isSpace `eq` T.stripEnd
prop_T_strip           = T.dropAround isSpace `eq` T.strip
prop_T_splitAt n       = L.splitAt n   `eqP` (unpack2 . T.splitAt n)
prop_TL_splitAt n      = L.splitAt n   `eqP` (unpack2 . TL.splitAt (fromIntegral n))
prop_T_span p          = L.span p      `eqP` (unpack2 . T.span p)
prop_TL_span p         = L.span p      `eqP` (unpack2 . TL.span p)
prop_T_break p         = L.break p     `eqP` (unpack2 . T.break p)
prop_TL_break p        = L.break p     `eqP` (unpack2 . TL.break p)
prop_T_group           = L.group       `eqP` (map unpackS . T.group)
prop_TL_group          = L.group       `eqP` (map unpackS . TL.group)
prop_T_groupBy p       = L.groupBy p   `eqP` (map unpackS . T.groupBy p)
prop_TL_groupBy p      = L.groupBy p   `eqP` (map unpackS . TL.groupBy p)
prop_T_inits           = L.inits       `eqP` (map unpackS . T.inits)
prop_TL_inits          = L.inits       `eqP` (map unpackS . TL.inits)
prop_T_tails           = L.tails       `eqP` (map unpackS . T.tails)
prop_TL_tails          = L.tails       `eqP` (map unpackS . TL.tails)

prop_T_split_i t       = id `eq` (T.intercalate t . T.split t)
prop_T_splitTimes_i k t = id `eq` (T.intercalate t . T.splitTimes k t)
prop_T_splitTimes_split k t = T.splitTimes k t `eq` \u ->
                              case L.splitAt k (T.split t u) of
                                (a,[]) -> a
                                (a,b)  -> a ++ [T.intercalate t b]
prop_T_splitTimesEnd_i k t = id `eq` (T.intercalate t . T.splitTimesEnd k t)
prop_T_splitTimesEnd_split t = T.splitTimesEnd maxBound t `eq` T.split t
prop_TL_split_i c      = id `eq` (TL.intercalate (TL.singleton c) . TL.split c)

prop_T_splitWith p     = splitWith p `eqP` (map unpackS . T.splitWith p)
prop_T_splitWith_count c = (L.length . T.splitWith (==c)) `eq` ((1+) . T.count c)
prop_T_splitWith_split c = T.splitWith (==c) `eq` T.split (T.singleton c)
prop_T_splitWith_Csplit c = T.splitWith (==c) `eq` C.split c
prop_TL_splitWith p    = splitWith p `eqP` (map unpackS . TL.splitWith p)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] =  [[]]
splitWith p xs = loop xs
    where loop s | null s'   = [l]
                 | otherwise = l : loop (tail s')
              where (l, s') = break p s

prop_T_chunksOf_same_lengths k = all ((==k) . T.length) . ini . T.chunksOf k
  where ini [] = []
        ini xs = init xs

prop_T_chunksOf_length k t = len == T.length t || (k <= 0 && len == 0)
  where len = L.sum . L.map T.length $ T.chunksOf k t

prop_T_breakSubstring_isInfixOf s l
                     = T.isInfixOf s l ==
                       T.null s || (not . T.null . snd $ C.breakSubstring s l)
prop_T_breakSubstringC c
                     = L.break (==c) `eqP`
                       (unpack2 . C.breakSubstring (T.singleton c))

prop_T_lines           = L.lines       `eqP` (map unpackS . T.lines)
prop_TL_lines          = L.lines       `eqP` (map unpackS . TL.lines)
{-
prop_T_lines'          = lines'        `eqP` (map unpackS . T.lines')
    where lines' "" =  []
          lines' s =  let (l, s') = break eol s
                      in  l : case s' of
                                []      -> []
                                ('\r':'\n':s'') -> lines' s''
                                (_:s'') -> lines' s''
          eol c = c == '\r' || c == '\n'
-}
prop_T_words           = L.words       `eqP` (map unpackS . T.words)

prop_TL_words          = L.words       `eqP` (map unpackS . TL.words)
prop_T_unlines         = unsquare (L.unlines `eq` (unpackS . T.unlines . map packS))
prop_TL_unlines        = unsquare (L.unlines `eq` (unpackS . TL.unlines . map packS))
prop_T_unwords         = unsquare (L.unwords `eq` (unpackS . T.unwords . map packS))
prop_TL_unwords        = unsquare (L.unwords `eq` (unpackS . TL.unwords . map packS))

prop_S_isPrefixOf s    = L.isPrefixOf s`eqP` (S.isPrefixOf (S.stream $ packS s) . S.stream)
prop_T_isPrefixOf s    = L.isPrefixOf s`eqP` T.isPrefixOf (packS s)
prop_TL_isPrefixOf s   = L.isPrefixOf s`eqP` TL.isPrefixOf (packS s)
prop_T_isSuffixOf s    = L.isSuffixOf s`eqP` T.isSuffixOf (packS s)
prop_TL_isSuffixOf s   = L.isSuffixOf s`eqP` TL.isSuffixOf (packS s)
prop_T_isInfixOf s     = L.isInfixOf s `eqP` T.isInfixOf (packS s)
prop_TL_isInfixOf s    = L.isInfixOf s `eqP` TL.isInfixOf (packS s)

prop_T_elem c          = L.elem c      `eqP` T.elem c
prop_TL_elem c         = L.elem c      `eqP` TL.elem c
prop_T_filter p        = L.filter p    `eqP` (unpackS . T.filter p)
prop_TL_filter p       = L.filter p    `eqP` (unpackS . TL.filter p)
prop_T_find p          = L.find p      `eqP` T.find p
prop_TL_find p         = L.find p      `eqP` TL.find p
prop_T_partition p     = L.partition p `eqP` (unpack2 . T.partition p)
prop_TL_partition p    = L.partition p `eqP` (unpack2 . TL.partition p)

prop_T_index s         = forAll (choose (-l,l*2)) ((s L.!!) `eq` T.index (packS s))
    where l = L.length s

prop_TL_index s        = forAll (choose (-l,l*2)) ((s L.!!) `eq` (TL.index (packS s) . fromIntegral))
    where l = L.length s

prop_T_findIndex p     = L.findIndex p `eqP` T.findIndex p
prop_TL_findIndex p    = (fmap fromIntegral . L.findIndex p) `eqP` TL.findIndex p
prop_T_findIndices p   = L.findIndices p `eqP` T.findIndices p
prop_TL_findIndices p  = (fmap fromIntegral . L.findIndices p) `eqP` TL.findIndices p
prop_T_elemIndex c     = L.elemIndex c `eqP` T.elemIndex c
prop_TL_elemIndex c    = (fmap fromIntegral . L.elemIndex c) `eqP` TL.elemIndex c
prop_T_elemIndices c   = L.elemIndices c`eqP` T.elemIndices c
prop_TL_elemIndices c  = (fmap fromIntegral . L.elemIndices c) `eqP` TL.elemIndices c
prop_T_count c         = (L.length . L.elemIndices c) `eqP` T.count c
prop_TL_count c        = (fromIntegral . L.length . L.elemIndices c) `eqP` TL.count c
prop_T_zip s           = L.zip s `eqP` T.zip (packS s)
prop_TL_zip s          = L.zip s `eqP` TL.zip (packS s)
prop_T_zipWith c s     = L.zipWith c s `eqP` (unpackS . T.zipWith c (packS s))
prop_TL_zipWith c s    = L.zipWith c s `eqP` (unpackS . TL.zipWith c (packS s))

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

main = defaultMain tests

tests = [
  testGroup "creation/elimination" [
    testProperty "t_pack_unpack" prop_T_pack_unpack,
    testProperty "tl_pack_unpack" prop_TL_pack_unpack,
    testProperty "t_stream_unstream" prop_T_stream_unstream,
    testProperty "tl_stream_unstream" prop_TL_stream_unstream,
    testProperty "t_reverse_stream" prop_T_reverse_stream,
    testProperty "t_singleton" prop_T_singleton,
    testProperty "tl_singleton" prop_TL_singleton,
    testProperty "tl_unstreamChunks" prop_TL_unstreamChunks,
    testProperty "tl_chunk_unchunk" prop_TL_chunk_unchunk
  ],

  testGroup "transcoding" [
    testProperty "t_ascii" prop_T_ascii,
    testProperty "t_utf8" prop_T_utf8,
    testProperty "tl_utf8" prop_TL_utf8,
    testProperty "t_utf16LE" prop_T_utf16LE,
    testProperty "t_utf16BE" prop_T_utf16BE,
    testProperty "t_utf32LE" prop_T_utf32LE,
    testProperty "t_utf32BE" prop_T_utf32BE
  ],

  testGroup "instances" [
    testProperty "s_Eq" prop_S_Eq,
    testProperty "sf_Eq" prop_Sf_Eq,
    testProperty "t_Eq" prop_T_Eq,
    testProperty "tl_Eq" prop_TL_Eq,
    testProperty "s_Ord" prop_S_Ord,
    testProperty "sf_Ord" prop_Sf_Ord,
    testProperty "t_Ord" prop_T_Ord,
    testProperty "tl_Ord" prop_TL_Ord,
    testProperty "t_Read" prop_T_Read,
    testProperty "tl_Read" prop_TL_Read,
    testProperty "t_Show" prop_T_Show,
    testProperty "tl_Show" prop_TL_Show,
    testProperty "t_mappend" prop_T_mappend,
    testProperty "tl_mappend" prop_TL_mappend,
    testProperty "t_mconcat" prop_T_mconcat,
    testProperty "tl_mconcat" prop_TL_mconcat,
    testProperty "t_IsString" prop_T_IsString,
    testProperty "tl_IsString" prop_TL_IsString
  ],

  testGroup "basics" [
    testProperty "s_cons" prop_S_cons,
    testProperty "sf_cons" prop_Sf_cons,
    testProperty "t_cons" prop_T_cons,
    testProperty "tl_cons" prop_TL_cons,
    testProperty "s_snoc" prop_S_snoc,
    testProperty "t_snoc" prop_T_snoc,
    testProperty "tl_snoc" prop_TL_snoc,
    testProperty "s_append" prop_S_append,
    testProperty "sf_append" prop_Sf_append,
    testProperty "t_append" prop_T_append,
    testProperty "t_uncons" prop_T_uncons,
    testProperty "tl_uncons" prop_TL_uncons,
    testProperty "s_head" prop_S_head,
    testProperty "t_head" prop_T_head,
    testProperty "tl_head" prop_TL_head,
    testProperty "s_last" prop_S_last,
    testProperty "t_last" prop_T_last,
    testProperty "tl_last" prop_TL_last,
    testProperty "s_tail" prop_S_tail,
    testProperty "t_tail" prop_T_tail,
    testProperty "tl_tail" prop_TL_tail,
    testProperty "s_init" prop_S_init,
    testProperty "t_init" prop_T_init,
    testProperty "tl_init" prop_TL_init,
    testProperty "s_null" prop_S_null,
    testProperty "t_null" prop_T_null,
    testProperty "tl_null" prop_TL_null,
    testProperty "s_length" prop_S_length,
    testProperty "t_length" prop_T_length,
    testProperty "tl_length" prop_TL_length
  ],

  testGroup "transformations" [
    testProperty "t_map" prop_T_map,
    testProperty "tl_map" prop_TL_map,
    testProperty "t_intercalate" prop_T_intercalate,
    testProperty "tl_intercalate" prop_TL_intercalate,
    testProperty "t_intersperse" prop_T_intersperse,
    testProperty "tl_intersperse" prop_TL_intersperse,
    testProperty "t_transpose" prop_T_transpose,
    testProperty "tl_transpose" prop_TL_transpose,
    testProperty "t_reverse" prop_T_reverse,
    testProperty "tl_reverse" prop_TL_reverse,
    testProperty "t_reverse_short" prop_T_reverse_short,
    testProperty "t_replace" prop_T_replace,

    testGroup "case conversion" [
      testProperty "t_toCaseFold_length" prop_T_toCaseFold_length,
      testProperty "t_toLower_length" prop_T_toLower_length,
      testProperty "t_toLower_lower" prop_T_toLower_lower,
      testProperty "t_toUpper_length" prop_T_toUpper_length,
      testProperty "t_toUpper_upper" prop_T_toUpper_upper
    ],

    testGroup "justification" [
      testProperty "s_justifyLeft" prop_S_justifyLeft,
      testProperty "t_justifyLeft" prop_T_justifyLeft,
      testProperty "t_justifyRight" prop_T_justifyRight
    ]
  ],

  testGroup "folds" [
    testProperty "t_foldl" prop_T_foldl,
    testProperty "tl_foldl" prop_TL_foldl,
    testProperty "t_foldl'" prop_T_foldl',
    testProperty "tl_foldl'" prop_TL_foldl',
    testProperty "t_foldl1" prop_T_foldl1,
    testProperty "tl_foldl1" prop_TL_foldl1,
    testProperty "t_foldl1'" prop_T_foldl1',
    testProperty "tl_foldl1'" prop_TL_foldl1',
    testProperty "t_foldr" prop_T_foldr,
    testProperty "tl_foldr" prop_TL_foldr,
    testProperty "t_foldr1" prop_T_foldr1,
    testProperty "tl_foldr1" prop_TL_foldr1,

    testGroup "special" [
      testProperty "t_concat" prop_T_concat,
      testProperty "tl_concat" prop_TL_concat,
      testProperty "t_concatMap" prop_T_concatMap,
      testProperty "tl_concatMap" prop_TL_concatMap,
      testProperty "t_any" prop_T_any,
      testProperty "tl_any" prop_TL_any,
      testProperty "t_all" prop_T_all,
      testProperty "tl_all" prop_TL_all,
      testProperty "t_maximum" prop_T_maximum,
      testProperty "tl_maximum" prop_TL_maximum,
      testProperty "t_minimum" prop_T_minimum,
      testProperty "tl_minimum" prop_TL_minimum
    ]
  ],

  testGroup "construction" [
    testGroup "scans" [
      testProperty "t_scanl" prop_T_scanl,
      testProperty "tl_scanl" prop_TL_scanl,
      testProperty "t_scanl1" prop_T_scanl1,
      testProperty "tl_scanl1" prop_TL_scanl1,
      testProperty "t_scanr" prop_T_scanr,
      testProperty "tl_scanr" prop_TL_scanr,
      testProperty "t_scanr1" prop_T_scanr1,
      testProperty "tl_scanr1" prop_TL_scanr1
    ],

    testGroup "mapAccum" [
      testProperty "t_mapAccumL" prop_T_mapAccumL,
      testProperty "tl_mapAccumL" prop_TL_mapAccumL,
      testProperty "t_mapAccumR" prop_T_mapAccumR,
      testProperty "tl_mapAccumR" prop_TL_mapAccumR
    ],

    testGroup "unfolds" [
      testProperty "t_replicate" prop_T_replicate,
      testProperty "tl_replicate" prop_TL_replicate,
      testProperty "t_unfoldr" prop_T_unfoldr,
      testProperty "tl_unfoldr" prop_TL_unfoldr,
      testProperty "t_unfoldrN" prop_T_unfoldrN,
      testProperty "tl_unfoldrN" prop_TL_unfoldrN
    ]
  ],

  testGroup "substrings" [
    testGroup "breaking" [
      testProperty "s_take" prop_S_take,
      testProperty "t_take" prop_T_take,
      testProperty "tl_take" prop_TL_take,
      testProperty "s_drop" prop_S_drop,
      testProperty "t_drop" prop_T_drop,
      testProperty "tl_drop" prop_TL_drop,
      testProperty "s_takeWhile" prop_S_takeWhile,
      testProperty "t_takeWhile" prop_T_takeWhile,
      testProperty "tl_takeWhile" prop_TL_takeWhile,
      testProperty "s_dropWhile" prop_S_dropWhile,
      testProperty "t_dropWhile" prop_T_dropWhile,
      testProperty "tl_dropWhile" prop_TL_dropWhile,
      testProperty "s_dropWhileEnd" prop_S_dropWhileEnd,
      testProperty "t_dropWhileEnd" prop_T_dropWhileEnd,
      testProperty "t_dropAround" prop_T_dropAround,
      testProperty "t_stripStart" prop_T_stripStart,
      testProperty "t_stripEnd" prop_T_stripEnd,
      testProperty "t_strip" prop_T_strip,
      testProperty "t_splitAt" prop_T_splitAt,
      testProperty "tl_splitAt" prop_TL_splitAt,
      testProperty "t_span" prop_T_span,
      testProperty "tl_span" prop_TL_span,
      testProperty "t_break" prop_T_break,
      testProperty "tl_break" prop_TL_break,
      testProperty "t_group" prop_T_group,
      testProperty "tl_group" prop_TL_group,
      testProperty "t_groupBy" prop_T_groupBy,
      testProperty "tl_groupBy" prop_TL_groupBy,
      testProperty "t_inits" prop_T_inits,
      testProperty "tl_inits" prop_TL_inits,
      testProperty "t_tails" prop_T_tails,
      testProperty "tl_tails" prop_TL_tails
    ],

    testGroup "breaking many" [
      testProperty "t_split_i" prop_T_split_i,
      testProperty "t_splitTimes_i" prop_T_splitTimes_i,
      testProperty "t_splitTimes_split" prop_T_splitTimes_split,
      testProperty "t_splitTimesEnd_i" prop_T_splitTimesEnd_i,
      testProperty "t_splitTimesEnd_split" prop_T_splitTimesEnd_split,
      testProperty "tl_split_i" prop_TL_split_i,
      testProperty "t_splitWith" prop_T_splitWith,
      testProperty "t_splitWith_count" prop_T_splitWith_count,
      testProperty "t_splitWith_split" prop_T_splitWith_split,
      testProperty "t_splitWith_Csplit" prop_T_splitWith_Csplit,
      testProperty "tl_splitWith" prop_TL_splitWith,
      testProperty "t_chunksOf_same_lengths" prop_T_chunksOf_same_lengths,
      testProperty "t_chunksOf_length" prop_T_chunksOf_length,
      testProperty "t_breakSubstringC" prop_T_breakSubstringC,
      testProperty "t_breakSubstring_isInfixOf" prop_T_breakSubstring_isInfixOf
    ],

    testGroup "lines and words" [
      testProperty "t_lines" prop_T_lines,
      testProperty "tl_lines" prop_TL_lines,
    --testProperty "t_lines'" prop_T_lines',
      testProperty "t_words" prop_T_words,
      testProperty "tl_words" prop_TL_words,
      testProperty "t_unlines" prop_T_unlines,
      testProperty "tl_unlines" prop_TL_unlines,
      testProperty "t_unwords" prop_T_unwords,
      testProperty "tl_unwords" prop_TL_unwords
    ]
  ],

  testGroup "predicates" [
    testProperty "s_isPrefixOf" prop_S_isPrefixOf,
    testProperty "t_isPrefixOf" prop_T_isPrefixOf,
    testProperty "tl_isPrefixOf" prop_TL_isPrefixOf,
    testProperty "t_isSuffixOf" prop_T_isSuffixOf,
    testProperty "tl_isSuffixOf" prop_TL_isSuffixOf,
    testProperty "t_isInfixOf" prop_T_isInfixOf,
    testProperty "tl_isInfixOf" prop_TL_isInfixOf
  ],

  testGroup "searching" [
    testProperty "t_elem" prop_T_elem,
    testProperty "tl_elem" prop_TL_elem,
    testProperty "t_filter" prop_T_filter,
    testProperty "tl_filter" prop_TL_filter,
    testProperty "t_find" prop_T_find,
    testProperty "tl_find" prop_TL_find,
    testProperty "t_partition" prop_T_partition,
    testProperty "tl_partition" prop_TL_partition
  ],

  testGroup "indexing" [
    testProperty "t_index" prop_T_index,
    testProperty "tl_index" prop_TL_index,
    testProperty "t_findIndex" prop_T_findIndex,
    testProperty "tl_findIndex" prop_TL_findIndex,
    testProperty "t_findIndices" prop_T_findIndices,
    testProperty "tl_findIndices" prop_TL_findIndices,
    testProperty "t_elemIndex" prop_T_elemIndex,
    testProperty "tl_elemIndex" prop_TL_elemIndex,
    testProperty "t_elemIndices" prop_T_elemIndices,
    testProperty "tl_elemIndices" prop_TL_elemIndices,
    testProperty "t_count" prop_T_count,
    testProperty "tl_count" prop_TL_count
  ],

  testGroup "zips" [
    testProperty "t_zip" prop_T_zip,
    testProperty "tl_zip" prop_TL_zip,
    testProperty "t_zipWith" prop_T_zipWith,
    testProperty "tl_zipWith" prop_TL_zipWith
  ],

  testGroup "regressions" [
    testProperty "s_filter_eq" prop_S_filter_eq
  ]
 ]
