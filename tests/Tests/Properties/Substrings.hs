-- | Tests for substring functions (@take@, @split@, @isInfixOf@, etc.)

{-# OPTIONS_GHC -fno-enable-rewrite-rules -fno-warn-missing-signatures #-}
module Tests.Properties.Substrings
    ( testSubstrings
    ) where

import Data.Char (isSpace)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck.Unicode (char)
import Tests.QuickCheckUtils
import Text.Show.Functions ()
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion as S
import qualified Data.Text.Internal.Fusion.Common as S
import qualified Data.Text.Internal.Lazy.Fusion as SL
import qualified Data.Text.Lazy as TL
import qualified Tests.SlowFunctions as Slow

s_take n          = L.take n      `eqP` (unpackS . S.take n)
s_take_s m        = L.take n      `eqP` (unpackS . S.unstream . S.take n)
  where n = small m
sf_take p n       = (L.take n . L.filter p) `eqP`
                    (unpackS . S.take n . S.filter p)
t_take n          = L.take n      `eqP` (unpackS . T.take n)
t_takeEnd n       = (L.reverse . L.take n . L.reverse) `eqP`
                    (unpackS . T.takeEnd n)
tl_take n         = L.take n      `eqP` (unpackS . TL.take (fromIntegral n))
tl_takeEnd n      = (L.reverse . L.take (fromIntegral n) . L.reverse) `eqP`
                    (unpackS . TL.takeEnd n)
s_drop n          = L.drop n      `eqP` (unpackS . S.drop n)
s_drop_s m        = L.drop n      `eqP` (unpackS . S.unstream . S.drop n)
  where n = small m
sf_drop p n       = (L.drop n . L.filter p) `eqP`
                    (unpackS . S.drop n . S.filter p)
t_drop n          = L.drop n      `eqP` (unpackS . T.drop n)
t_dropEnd n       = (L.reverse . L.drop n . L.reverse) `eqP`
                    (unpackS . T.dropEnd n)
tl_drop n         = L.drop n      `eqP` (unpackS . TL.drop (fromIntegral n))
tl_dropEnd n      = (L.reverse . L.drop n . L.reverse) `eqP`
                    (unpackS . TL.dropEnd (fromIntegral n))
s_take_drop m     = (L.take n . L.drop n) `eqP` (unpackS . S.take n . S.drop n)
  where n = small m
s_take_drop_s m   = (L.take n . L.drop n) `eqP`
                    (unpackS . S.unstream . S.take n . S.drop n)
  where n = small m
s_takeWhile p     = L.takeWhile p `eqP` (unpackS . S.takeWhile p)
s_takeWhile_s p   = L.takeWhile p `eqP` (unpackS . S.unstream . S.takeWhile p)
sf_takeWhile q p  = (L.takeWhile p . L.filter q) `eqP`
                    (unpackS . S.takeWhile p . S.filter q)
noMatch = do
  c <- char
  d <- suchThat char (/= c)
  return (c,d)
t_takeWhile p     = L.takeWhile p `eqP` (unpackS . T.takeWhile p)
tl_takeWhile p    = L.takeWhile p `eqP` (unpackS . TL.takeWhile p)
t_takeWhileEnd p  = (L.reverse . L.takeWhile p . L.reverse) `eqP`
                    (unpackS . T.takeWhileEnd p)
t_takeWhileEnd_null t = forAll noMatch $ \(c,d) -> T.null $
                    T.takeWhileEnd (==d) (T.snoc t c)
tl_takeWhileEnd p = (L.reverse . L.takeWhile p . L.reverse) `eqP`
                    (unpackS . TL.takeWhileEnd p)
tl_takeWhileEnd_null t = forAll noMatch $ \(c,d) -> TL.null $
                    TL.takeWhileEnd (==d) (TL.snoc t c)
s_dropWhile p     = L.dropWhile p `eqP` (unpackS . S.dropWhile p)
s_dropWhile_s p   = L.dropWhile p `eqP` (unpackS . S.unstream . S.dropWhile p)
sf_dropWhile q p  = (L.dropWhile p . L.filter q) `eqP`
                    (unpackS . S.dropWhile p . S.filter q)
t_dropWhile p     = L.dropWhile p `eqP` (unpackS . T.dropWhile p)
tl_dropWhile p    = L.dropWhile p `eqP` (unpackS . S.dropWhile p)
t_dropWhileEnd p  = (L.reverse . L.dropWhile p . L.reverse) `eqP`
                    (unpackS . T.dropWhileEnd p)
tl_dropWhileEnd p = (L.reverse . L.dropWhile p . L.reverse) `eqP`
                    (unpackS . TL.dropWhileEnd p)
t_dropAround p    = (L.dropWhile p . L.reverse . L.dropWhile p . L.reverse)
                    `eqP` (unpackS . T.dropAround p)
tl_dropAround p   = (L.dropWhile p . L.reverse . L.dropWhile p . L.reverse)
                    `eqP` (unpackS . TL.dropAround p)
t_stripStart      = T.dropWhile isSpace `eq` T.stripStart
tl_stripStart     = TL.dropWhile isSpace `eq` TL.stripStart
t_stripEnd        = T.dropWhileEnd isSpace `eq` T.stripEnd
tl_stripEnd       = TL.dropWhileEnd isSpace `eq` TL.stripEnd
t_strip           = T.dropAround isSpace `eq` T.strip
tl_strip          = TL.dropAround isSpace `eq` TL.strip
t_splitAt n       = L.splitAt n   `eqP` (unpack2 . T.splitAt n)
tl_splitAt n      = L.splitAt n   `eqP` (unpack2 . TL.splitAt (fromIntegral n))
t_span p        = L.span p      `eqP` (unpack2 . T.span p)
tl_span p       = L.span p      `eqP` (unpack2 . TL.span p)

t_breakOn_id s      = squid `eq` (uncurry T.append . T.breakOn s)
  where squid t | T.null s  = error "empty"
                | otherwise = t
tl_breakOn_id s     = squid `eq` (uncurry TL.append . TL.breakOn s)
  where squid t | TL.null s  = error "empty"
                | otherwise = t
t_breakOn_start (NotEmpty s) t =
    let (k,m) = T.breakOn s t
    in k `T.isPrefixOf` t && (T.null m || s `T.isPrefixOf` m)
tl_breakOn_start (NotEmpty s) t =
    let (k,m) = TL.breakOn s t
    in k `TL.isPrefixOf` t && TL.null m || s `TL.isPrefixOf` m
t_breakOnEnd_end (NotEmpty s) t =
    let (m,k) = T.breakOnEnd s t
    in k `T.isSuffixOf` t && (T.null m || s `T.isSuffixOf` m)
tl_breakOnEnd_end (NotEmpty s) t =
    let (m,k) = TL.breakOnEnd s t
    in k `TL.isSuffixOf` t && (TL.null m || s `TL.isSuffixOf` m)
t_break p       = L.break p     `eqP` (unpack2 . T.break p)
tl_break p      = L.break p     `eqP` (unpack2 . TL.break p)
t_group           = L.group       `eqP` (map unpackS . T.group)
tl_group          = L.group       `eqP` (map unpackS . TL.group)
t_groupBy p       = L.groupBy p   `eqP` (map unpackS . T.groupBy p)
tl_groupBy p      = L.groupBy p   `eqP` (map unpackS . TL.groupBy p)
t_inits           = L.inits       `eqP` (map unpackS . T.inits)
tl_inits          = L.inits       `eqP` (map unpackS . TL.inits)
t_tails           = L.tails       `eqP` (map unpackS . T.tails)
tl_tails          = unsquare $
                    L.tails       `eqP` (map unpackS . TL.tails)
t_findAppendId = unsquare $ \(NotEmpty s) ts ->
    let t = T.intercalate s ts
    in all (==t) $ map (uncurry T.append) (T.breakOnAll s t)
tl_findAppendId = unsquare $ \(NotEmpty s) ts ->
    let t = TL.intercalate s ts
    in all (==t) $ map (uncurry TL.append) (TL.breakOnAll s t)
t_findContains = unsquare $ \(NotEmpty s) ->
    all (T.isPrefixOf s . snd) . T.breakOnAll s . T.intercalate s
tl_findContains = unsquare $ \(NotEmpty s) -> all (TL.isPrefixOf s . snd) .
                               TL.breakOnAll s . TL.intercalate s
sl_filterCount c  = (L.genericLength . L.filter (==c)) `eqP` SL.countChar c
t_findCount s     = (L.length . T.breakOnAll s) `eq` T.count s
tl_findCount s    = (L.genericLength . TL.breakOnAll s) `eq` TL.count s

t_splitOn_split s  = unsquare $
                     (T.splitOn s `eq` Slow.splitOn s) . T.intercalate s
tl_splitOn_split s = unsquare $
                     ((TL.splitOn (TL.fromStrict s) . TL.fromStrict) `eq`
                      (map TL.fromStrict . T.splitOn s)) . T.intercalate s
t_splitOn_i (NotEmpty t)  = id `eq` (T.intercalate t . T.splitOn t)
tl_splitOn_i (NotEmpty t) = id `eq` (TL.intercalate t . TL.splitOn t)

t_split p       = split p `eqP` (map unpackS . T.split p)
t_split_count c = (L.length . T.split (==c)) `eq`
                  ((1+) . T.count (T.singleton c))
t_split_splitOn c = T.split (==c) `eq` T.splitOn (T.singleton c)
tl_split p      = split p `eqP` (map unpackS . TL.split p)

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] =  [[]]
split p xs = loop xs
    where loop s | null s'   = [l]
                 | otherwise = l : loop (tail s')
              where (l, s') = break p s

t_chunksOf_same_lengths k = all ((==k) . T.length) . ini . T.chunksOf k
  where ini [] = []
        ini xs = init xs

t_chunksOf_length k t = len == T.length t || (k <= 0 && len == 0)
  where len = L.sum . L.map T.length $ T.chunksOf k t

tl_chunksOf k = T.chunksOf k `eq` (map (T.concat . TL.toChunks) .
                                   TL.chunksOf (fromIntegral k) . TL.fromStrict)

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
t_unlines         = unsquare $
                    L.unlines `eq` (unpackS . T.unlines . map packS)
tl_unlines        = unsquare $
                    L.unlines `eq` (unpackS . TL.unlines . map packS)
t_unwords         = unsquare $
                    L.unwords `eq` (unpackS . T.unwords . map packS)
tl_unwords        = unsquare $
                    L.unwords `eq` (unpackS . TL.unwords . map packS)

s_isPrefixOf s    = L.isPrefixOf s `eqP`
                    (S.isPrefixOf (S.stream $ packS s) . S.stream)
sf_isPrefixOf p s = (L.isPrefixOf s . L.filter p) `eqP`
                    (S.isPrefixOf (S.stream $ packS s) . S.filter p . S.stream)
t_isPrefixOf s    = L.isPrefixOf s`eqP` T.isPrefixOf (packS s)
tl_isPrefixOf s   = L.isPrefixOf s`eqP` TL.isPrefixOf (packS s)
t_isSuffixOf s    = L.isSuffixOf s`eqP` T.isSuffixOf (packS s)
tl_isSuffixOf s   = L.isSuffixOf s`eqP` TL.isSuffixOf (packS s)
t_isInfixOf s     = L.isInfixOf s `eqP` T.isInfixOf (packS s)
tl_isInfixOf s    = L.isInfixOf s `eqP` TL.isInfixOf (packS s)

t_stripPrefix s      = (fmap packS . L.stripPrefix s) `eqP` T.stripPrefix (packS s)
tl_stripPrefix s     = (fmap packS . L.stripPrefix s) `eqP` TL.stripPrefix (packS s)

stripSuffix p t = reverse `fmap` L.stripPrefix (reverse p) (reverse t)

t_stripSuffix s      = (fmap packS . stripSuffix s) `eqP` T.stripSuffix (packS s)
tl_stripSuffix s     = (fmap packS . stripSuffix s) `eqP` TL.stripSuffix (packS s)

commonPrefixes a0@(_:_) b0@(_:_) = Just (go a0 b0 [])
    where go (a:as) (b:bs) ps
              | a == b = go as bs (a:ps)
          go as bs ps  = (reverse ps,as,bs)
commonPrefixes _ _ = Nothing

t_commonPrefixes a b (NonEmpty p)
    = commonPrefixes pa pb ==
      repack `fmap` T.commonPrefixes (packS pa) (packS pb)
  where repack (x,y,z) = (unpackS x,unpackS y,unpackS z)
        pa = p ++ a
        pb = p ++ b

tl_commonPrefixes a b (NonEmpty p)
    = commonPrefixes pa pb ==
      repack `fmap` TL.commonPrefixes (packS pa) (packS pb)
  where repack (x,y,z) = (unpackS x,unpackS y,unpackS z)
        pa = p ++ a
        pb = p ++ b

testSubstrings :: TestTree
testSubstrings =
  testGroup "substrings" [
    testGroup "breaking" [
      testProperty "s_take" s_take,
      testProperty "s_take_s" s_take_s,
      testProperty "sf_take" sf_take,
      testProperty "t_take" t_take,
      testProperty "t_takeEnd" t_takeEnd,
      testProperty "tl_take" tl_take,
      testProperty "tl_takeEnd" tl_takeEnd,
      testProperty "s_drop" s_drop,
      testProperty "s_drop_s" s_drop_s,
      testProperty "sf_drop" sf_drop,
      testProperty "t_drop" t_drop,
      testProperty "t_dropEnd" t_dropEnd,
      testProperty "tl_drop" tl_drop,
      testProperty "tl_dropEnd" tl_dropEnd,
      testProperty "s_take_drop" s_take_drop,
      testProperty "s_take_drop_s" s_take_drop_s,
      testProperty "s_takeWhile" s_takeWhile,
      testProperty "s_takeWhile_s" s_takeWhile_s,
      testProperty "sf_takeWhile" sf_takeWhile,
      testProperty "t_takeWhile" t_takeWhile,
      testProperty "tl_takeWhile" tl_takeWhile,
      testProperty "t_takeWhileEnd" t_takeWhileEnd,
      testProperty "t_takeWhileEnd_null" t_takeWhileEnd_null,
      testProperty "tl_takeWhileEnd" tl_takeWhileEnd,
      testProperty "tl_takeWhileEnd_null" tl_takeWhileEnd_null,
      testProperty "sf_dropWhile" sf_dropWhile,
      testProperty "s_dropWhile" s_dropWhile,
      testProperty "s_dropWhile_s" s_dropWhile_s,
      testProperty "t_dropWhile" t_dropWhile,
      testProperty "tl_dropWhile" tl_dropWhile,
      testProperty "t_dropWhileEnd" t_dropWhileEnd,
      testProperty "tl_dropWhileEnd" tl_dropWhileEnd,
      testProperty "t_dropAround" t_dropAround,
      testProperty "tl_dropAround" tl_dropAround,
      testProperty "t_stripStart" t_stripStart,
      testProperty "tl_stripStart" tl_stripStart,
      testProperty "t_stripEnd" t_stripEnd,
      testProperty "tl_stripEnd" tl_stripEnd,
      testProperty "t_strip" t_strip,
      testProperty "tl_strip" tl_strip,
      testProperty "t_splitAt" t_splitAt,
      testProperty "tl_splitAt" tl_splitAt,
      testProperty "t_span" t_span,
      testProperty "tl_span" tl_span,
      testProperty "t_breakOn_id" t_breakOn_id,
      testProperty "tl_breakOn_id" tl_breakOn_id,
      testProperty "t_breakOn_start" t_breakOn_start,
      testProperty "tl_breakOn_start" tl_breakOn_start,
      testProperty "t_breakOnEnd_end" t_breakOnEnd_end,
      testProperty "tl_breakOnEnd_end" tl_breakOnEnd_end,
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
      testProperty "t_findAppendId" t_findAppendId,
      testProperty "tl_findAppendId" tl_findAppendId,
      testProperty "t_findContains" t_findContains,
      testProperty "tl_findContains" tl_findContains,
      testProperty "sl_filterCount" sl_filterCount,
      testProperty "t_findCount" t_findCount,
      testProperty "tl_findCount" tl_findCount,
      testProperty "t_splitOn_split" t_splitOn_split,
      testProperty "tl_splitOn_split" tl_splitOn_split,
      testProperty "t_splitOn_i" t_splitOn_i,
      testProperty "tl_splitOn_i" tl_splitOn_i,
      testProperty "t_split" t_split,
      testProperty "t_split_count" t_split_count,
      testProperty "t_split_splitOn" t_split_splitOn,
      testProperty "tl_split" tl_split,
      testProperty "t_chunksOf_same_lengths" t_chunksOf_same_lengths,
      testProperty "t_chunksOf_length" t_chunksOf_length,
      testProperty "tl_chunksOf" tl_chunksOf
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
    ],

    testGroup "predicates" [
      testProperty "s_isPrefixOf" s_isPrefixOf,
      testProperty "sf_isPrefixOf" sf_isPrefixOf,
      testProperty "t_isPrefixOf" t_isPrefixOf,
      testProperty "tl_isPrefixOf" tl_isPrefixOf,
      testProperty "t_isSuffixOf" t_isSuffixOf,
      testProperty "tl_isSuffixOf" tl_isSuffixOf,
      testProperty "t_isInfixOf" t_isInfixOf,
      testProperty "tl_isInfixOf" tl_isInfixOf,

      testGroup "view" [
        testProperty "t_stripPrefix" t_stripPrefix,
        testProperty "tl_stripPrefix" tl_stripPrefix,
        testProperty "t_stripSuffix" t_stripSuffix,
        testProperty "tl_stripSuffix" tl_stripSuffix,
        testProperty "t_commonPrefixes" t_commonPrefixes,
        testProperty "tl_commonPrefixes" tl_commonPrefixes
      ]
    ]
  ]
