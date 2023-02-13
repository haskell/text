-- | Tests for operations that don't fit in the other @Test.Properties.*@ modules.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC  -fno-warn-missing-signatures #-}

module Tests.Properties.Text
    ( testText
    ) where

import Data.Char (isLower, isLetter, isUpper)
import Data.Maybe (mapMaybe)
import Data.Text.Internal.Fusion.Size
import Data.Word (Word8)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Tests.QuickCheckUtils
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion as S
import qualified Data.Text.Internal.Fusion.Common as S
import qualified Data.Text.Internal.Lazy.Fusion as SL
import qualified Data.Text.Internal.Lazy.Search as S (indices)
import qualified Data.Text.Internal.Search as T (indices)
import qualified Data.Text.Lazy as TL
import qualified Tests.SlowFunctions as Slow

t_pack_unpack       = (T.unpack . T.pack) `eq` id
tl_pack_unpack      = (TL.unpack . TL.pack) `eq` id
t_stream_unstream   = (S.unstream . S.stream) `eq` id
tl_stream_unstream  = (SL.unstream . SL.stream) `eq` id
t_reverse_stream t  = (S.reverse . S.reverseStream) t === t
t_singleton c       = [c] === (T.unpack . T.singleton) c
tl_singleton c      = [c] === (TL.unpack . TL.singleton) c
tl_unstreamChunks x = f 11 x === f 1000 x
    where f n = SL.unstreamChunks n . S.streamList
tl_chunk_unchunk    = (TL.fromChunks . TL.toChunks) `eq` id
tl_from_to_strict   = (TL.fromStrict . TL.toStrict) `eq` id

s_map (applyFun -> f)   = map f  `eqP` (unpackS . S.map f)
s_map_s (applyFun -> f) = map f  `eqP` (unpackS . S.unstream . S.map f)
sf_map (applyFun -> p) (applyFun -> f) = (map f . L.filter p)  `eqP` (unpackS . S.map f . S.filter p)

t_map (applyFun -> f)                      = map f  `eqP` (unpackS . T.map f)
tl_map (applyFun -> f)                     = map f  `eqP` (unpackS . TL.map f)
t_map_map (applyFun -> f) (applyFun -> g)  = (map f . map g) `eqP` (unpackS . T.map f . T.map g)
tl_map_map (applyFun -> f) (applyFun -> g) = (map f . map g)  `eqP` (unpackS . TL.map f . TL.map g)
t_length_map (applyFun -> f)               = (L.length . map f)  `eqP` (T.length . T.map f)
tl_length_map (applyFun -> f)              = (L.genericLength . map f)  `eqP` (TL.length . TL.map f)

s_intercalate c   = (L.intercalate c . unSqrt) `eq`
                    (unpackS . S.intercalate (packS c) . map packS . unSqrt)
t_intercalate c   = (L.intercalate c . unSqrt) `eq`
                    (unpackS . T.intercalate (packS c) . map packS . unSqrt)
tl_intercalate c  = (L.intercalate c . unSqrt) `eq`
                    (unpackS . TL.intercalate (TL.pack c) . map TL.pack . unSqrt)
t_length_intercalate c  = (L.length . L.intercalate c . unSqrt) `eq`
                    (T.length . T.intercalate (packS c) . map packS . unSqrt)
tl_length_intercalate c = (L.genericLength . L.intercalate c . unSqrt) `eq`
                    (TL.length . TL.intercalate (TL.pack c) . map TL.pack . unSqrt)
s_intersperse c   = L.intersperse c `eqP`
                    (unpackS . S.intersperse c)
s_intersperse_s c = L.intersperse c `eqP`
                    (unpackS . S.unstream . S.intersperse c)
sf_intersperse (applyFun -> p) c
                  = (L.intersperse c . L.filter p) `eqP`
                   (unpackS . S.intersperse c . S.filter p)
t_intersperse c   = L.intersperse c `eqPSqrt` (unpackS . T.intersperse c)
tl_intersperse c  = L.intersperse c `eqPSqrt` (unpackS . TL.intersperse c)
t_length_intersperse c  = (L.length . L.intersperse c) `eqPSqrt` (T.length . T.intersperse c)
tl_length_intersperse c = (L.genericLength . L.intersperse c) `eqPSqrt` (TL.length . TL.intersperse c)
t_transpose       = (L.transpose . unSqrt) `eq` (map unpackS . T.transpose . map packS . unSqrt)
tl_transpose      = (L.transpose . unSqrt) `eq` (map unpackS . TL.transpose . map TL.pack . unSqrt)
t_reverse         = L.reverse `eqP` (unpackS . T.reverse)
tl_reverse        = L.reverse `eqP` (unpackS . TL.reverse)
t_reverse_short n = L.reverse `eqP` (unpackS . S.reverse . shorten n . S.stream)

t_replace s d     = (L.intercalate d . splitOn s) `eqP`
                    (unpackS . T.replace (T.pack s) (T.pack d))
tl_replace s d     = (L.intercalate d . splitOn s) `eqP`
                     (unpackS . TL.replace (TL.pack s) (TL.pack d))

splitOn :: (Eq a) => [a] -> [a] -> [[a]]
splitOn pat src0
    | l == 0    = error "splitOn: empty"
    | otherwise = go src0
  where
    l           = length pat
    go src      = search 0 src
      where
        search _ [] = [src]
        search !n s@(_:s')
            | pat `L.isPrefixOf` s = take n src : go (drop l s)
            | otherwise            = search (n+1) s'

s_toCaseFold_length xs = S.length (S.toCaseFold s) >= length xs
    where s = S.streamList xs
sf_toCaseFold_length (applyFun -> p) xs =
    (S.length . S.toCaseFold . S.filter p $ s) >= (length . L.filter p $ xs)
    where s = S.streamList xs
t_toCaseFold_length t = T.length (T.toCaseFold t) >= T.length t

tl_toCaseFold_length t = TL.length (TL.toCaseFold t) >= TL.length t
#if MIN_VERSION_base(4,16,0)
t_toCaseFold_char c = c `notElem` (toCaseFoldExceptions ++ cherokeeLower ++ cherokeeUpper) ==>
    T.toCaseFold (T.singleton c) === T.singleton (C.toLower c)
#endif

-- | Baseline generated with GHC 9.2 + text-1.2.5.0,
t_toCaseFold_exceptions = T.unpack (T.toCaseFold (T.pack toCaseFoldExceptions)) === "\956ssi\775\700nsj\780\953\953\776\769\965\776\769\963\946\952\966\960\954\961\949\1381\1410\5104\5105\5106\5107\5108\5109\1074\1076\1086\1089\1090\1090\1098\1123\42571h\817t\776w\778y\778a\702\7777ss\965\787\965\787\768\965\787\769\965\787\834\7936\953\7937\953\7938\953\7939\953\7940\953\7941\953\7942\953\7943\953\7936\953\7937\953\7938\953\7939\953\7940\953\7941\953\7942\953\7943\953\7968\953\7969\953\7970\953\7971\953\7972\953\7973\953\7974\953\7975\953\7968\953\7969\953\7970\953\7971\953\7972\953\7973\953\7974\953\7975\953\8032\953\8033\953\8034\953\8035\953\8036\953\8037\953\8038\953\8039\953\8032\953\8033\953\8034\953\8035\953\8036\953\8037\953\8038\953\8039\953\8048\953\945\953\940\953\945\834\945\834\953\945\953\953\8052\953\951\953\942\953\951\834\951\834\953\951\953\953\776\768\953\776\769\953\834\953\776\834\965\776\768\965\776\769\961\787\965\834\965\776\834\8060\953\969\953\974\953\969\834\969\834\953\969\953fffiflffifflstst\1396\1398\1396\1381\1396\1387\1406\1398\1396\1389"
t_toCaseFold_cherokeeLower = T.all (`elem` cherokeeUpper) (T.toCaseFold (T.pack cherokeeLower))
t_toCaseFold_cherokeeUpper = conjoin $
    map (\c -> T.toCaseFold (T.singleton c) === T.singleton c) cherokeeUpper

-- | Generated with GHC 9.2 + text-1.2.5.0,
-- filter (\c -> c `notElem` (cherokeeUpper ++ cherokeeLower)) $
--   filter (\c -> T.toCaseFold (T.singleton c) /= T.singleton (Data.Char.toLower c))
--     [minBound .. maxBound]
toCaseFoldExceptions = "\181\223\304\329\383\496\837\912\944\962\976\977\981\982\1008\1009\1013\1415\5112\5113\5114\5115\5116\5117\7296\7297\7298\7299\7300\7301\7302\7303\7304\7830\7831\7832\7833\7834\7835\7838\8016\8018\8020\8022\8064\8065\8066\8067\8068\8069\8070\8071\8072\8073\8074\8075\8076\8077\8078\8079\8080\8081\8082\8083\8084\8085\8086\8087\8088\8089\8090\8091\8092\8093\8094\8095\8096\8097\8098\8099\8100\8101\8102\8103\8104\8105\8106\8107\8108\8109\8110\8111\8114\8115\8116\8118\8119\8124\8126\8130\8131\8132\8134\8135\8140\8146\8147\8150\8151\8162\8163\8164\8166\8167\8178\8179\8180\8182\8183\8188\64256\64257\64258\64259\64260\64261\64262\64275\64276\64277\64278\64279"
cherokeeUpper = ['\x13A0'..'\x13F7'] -- x13F8..13FF are lowercase
cherokeeLower = ['\xAB70'..'\xABBF']

t_toLower_length t = T.length (T.toLower t) >= T.length t
t_toLower_lower t = p (T.toLower t) >= p t
    where p = T.length . T.filter isLower
tl_toLower_lower t = p (TL.toLower t) >= p t
    where p = TL.length . TL.filter isLower
#if MIN_VERSION_base(4,13,0)
t_toLower_char c = c /= '\304' ==>
    T.toLower (T.singleton c) === T.singleton (C.toLower c)
#endif
t_toLower_dotted_i = T.unpack (T.toLower (T.singleton '\304')) === "i\775"

t_toUpper_length t = T.length (T.toUpper t) >= T.length t
t_toUpper_upper t = p (T.toUpper t) >= p t
    where p = T.length . T.filter isUpper
tl_toUpper_upper t = p (TL.toUpper t) >= p t
    where p = TL.length . TL.filter isUpper
#if MIN_VERSION_base(4,13,0)
t_toUpper_char c = c `notElem` toUpperExceptions ==>
    T.toUpper (T.singleton c) === T.singleton (C.toUpper c)
#endif

-- | Baseline generated with GHC 9.2 + text-1.2.5.0,
t_toUpper_exceptions = T.unpack (T.toUpper (T.pack toUpperExceptions)) === "SS\700NJ\780\921\776\769\933\776\769\1333\1362H\817T\776W\778Y\778A\702\933\787\933\787\768\933\787\769\933\787\834\7944\921\7945\921\7946\921\7947\921\7948\921\7949\921\7950\921\7951\921\7944\921\7945\921\7946\921\7947\921\7948\921\7949\921\7950\921\7951\921\7976\921\7977\921\7978\921\7979\921\7980\921\7981\921\7982\921\7983\921\7976\921\7977\921\7978\921\7979\921\7980\921\7981\921\7982\921\7983\921\8040\921\8041\921\8042\921\8043\921\8044\921\8045\921\8046\921\8047\921\8040\921\8041\921\8042\921\8043\921\8044\921\8045\921\8046\921\8047\921\8122\921\913\921\902\921\913\834\913\834\921\913\921\8138\921\919\921\905\921\919\834\919\834\921\919\921\921\776\768\921\776\769\921\834\921\776\834\933\776\768\933\776\769\929\787\933\834\933\776\834\8186\921\937\921\911\921\937\834\937\834\921\937\921FFFIFLFFIFFLSTST\1348\1350\1348\1333\1348\1339\1358\1350\1348\1341"

-- | Generated with GHC 9.2 + text-1.2.5.0,
-- filter (\c -> T.toUpper (T.singleton c) /= T.singleton (Data.Char.toUpper c))
--   [minBound .. maxBound]
toUpperExceptions = "\223\329\496\912\944\1415\7830\7831\7832\7833\7834\8016\8018\8020\8022\8064\8065\8066\8067\8068\8069\8070\8071\8072\8073\8074\8075\8076\8077\8078\8079\8080\8081\8082\8083\8084\8085\8086\8087\8088\8089\8090\8091\8092\8093\8094\8095\8096\8097\8098\8099\8100\8101\8102\8103\8104\8105\8106\8107\8108\8109\8110\8111\8114\8115\8116\8118\8119\8124\8130\8131\8132\8134\8135\8140\8146\8147\8150\8151\8162\8163\8164\8166\8167\8178\8179\8180\8182\8183\8188\64256\64257\64258\64259\64260\64261\64262\64275\64276\64277\64278\64279"

t_toTitle_title t = all (<= 1) (caps w)
    where caps = fmap (T.length . T.filter isUpper) . T.words . T.toTitle
          -- TIL: there exist uppercase-only letters
          w = T.filter (\c -> if C.isUpper c then C.toLower c /= c else True) t
t_toTitle_1stNotLower = and . notLow . T.toTitle . T.filter stable . T.filter (not . isGeorgian)
    where notLow = mapMaybe (fmap (not . isLower) . (T.find isLetter)) . T.words
          -- Surprise! The Spanish/Portuguese ordinal indicators changed
          -- from category Ll (letter, lowercase) to Lo (letter, other)
          -- in Unicode 7.0
          -- Oh, and there exist lowercase-only letters (see previous test)
          stable c = if isLower c
                     then C.toUpper c /= c
                     else c /= '\170' && c /= '\186'
          -- Georgian text does not have a concept of title case
          -- https://en.wikipedia.org/wiki/Georgian_Extended
          isGeorgian c = c >= '\4256' && c < '\4352'
#if MIN_VERSION_base(4,13,0)
t_toTitle_char c = c `notElem` toTitleExceptions ==>
    T.toTitle (T.singleton c) === T.singleton (C.toUpper c)
#endif

-- | Baseline generated with GHC 9.2 + text-1.2.5.0,
t_toTitle_exceptions = T.unpack (T.concatMap (T.toTitle . T.singleton) (T.pack toTitleExceptions)) === "Ss\700N\453\453\453\456\456\456\459\459\459J\780\498\498\498\921\776\769\933\776\769\1333\1410\4304\4305\4306\4307\4308\4309\4310\4311\4312\4313\4314\4315\4316\4317\4318\4319\4320\4321\4322\4323\4324\4325\4326\4327\4328\4329\4330\4331\4332\4333\4334\4335\4336\4337\4338\4339\4340\4341\4342\4343\4344\4345\4346\4349\4350\4351H\817T\776W\778Y\778A\702\933\787\933\787\768\933\787\769\933\787\834\8122\837\902\837\913\834\913\834\837\8138\837\905\837\919\834\919\834\837\921\776\768\921\776\769\921\834\921\776\834\933\776\768\933\776\769\929\787\933\834\933\776\834\8186\837\911\837\937\834\937\834\837FfFiFlFfiFflStSt\1348\1398\1348\1381\1348\1387\1358\1398\1348\1389"

-- | Generated with GHC 9.2 + text-1.2.5.0,
-- filter (\c -> T.toTitle (T.singleton c) /= T.singleton (Data.Char.toUpper c))
--   [minBound .. maxBound]
toTitleExceptions = "\223\329\452\453\454\455\456\457\458\459\460\496\497\498\499\912\944\1415\4304\4305\4306\4307\4308\4309\4310\4311\4312\4313\4314\4315\4316\4317\4318\4319\4320\4321\4322\4323\4324\4325\4326\4327\4328\4329\4330\4331\4332\4333\4334\4335\4336\4337\4338\4339\4340\4341\4342\4343\4344\4345\4346\4349\4350\4351\7830\7831\7832\7833\7834\8016\8018\8020\8022\8114\8116\8118\8119\8130\8132\8134\8135\8146\8147\8150\8151\8162\8163\8164\8166\8167\8178\8180\8182\8183\64256\64257\64258\64259\64260\64261\64262\64275\64276\64277\64278\64279"

t_toUpper_idempotent t = T.toUpper (T.toUpper t) === T.toUpper t
t_toLower_idempotent t = T.toLower (T.toLower t) === T.toLower t
t_toCaseFold_idempotent t = T.toCaseFold (T.toCaseFold t) === T.toCaseFold t

ascii_toLower (ASCIIString xs) = map C.toLower xs === T.unpack (T.toLower (T.pack xs))
ascii_toUpper (ASCIIString xs) = map C.toUpper xs === T.unpack (T.toUpper (T.pack xs))
ascii_toCaseFold (ASCIIString xs) = map C.toLower xs === T.unpack (T.toCaseFold (T.pack xs))

ascii_toTitle (ASCIIString xs) = referenceToTitle False xs === T.unpack (T.toTitle (T.pack xs))
  where
    referenceToTitle _ [] = []
    referenceToTitle False (y : ys)
      | C.isLetter y = C.toUpper y : referenceToTitle True ys
      | otherwise = y : referenceToTitle False ys
    referenceToTitle True (y : ys)
      | C.isLetter y = C.toLower y : referenceToTitle True ys
      | otherwise = y : referenceToTitle (not (C.isSpace y)) ys

justifyLeft k c xs  = xs ++ L.replicate (k - length xs) c
justifyRight m n xs = L.replicate (m - length xs) n ++ xs
center k c xs
    | len >= k  = xs
    | otherwise = L.replicate l c ++ xs ++ L.replicate r c
   where len = length xs
         d   = k - len
         r   = d `div` 2
         l   = d - r

s_justifyLeft k c = justifyLeft j c `eqP` (unpackS . S.justifyLeftI j c)
    where j = fromIntegral (k :: Word8)
s_justifyLeft_s k c = justifyLeft j c `eqP`
                      (unpackS . S.unstream . S.justifyLeftI j c)
    where j = fromIntegral (k :: Word8)
sf_justifyLeft (applyFun -> p) k c
                    = (justifyLeft j c . L.filter p) `eqP`
                       (unpackS . S.justifyLeftI j c . S.filter p)
    where j = fromIntegral (k :: Word8)
t_justifyLeft k c = justifyLeft j c `eqP` (unpackS . T.justifyLeft j c)
    where j = fromIntegral (k :: Word8)
tl_justifyLeft k c = justifyLeft j c `eqP`
                     (unpackS . TL.justifyLeft (fromIntegral j) c)
    where j = fromIntegral (k :: Word8)
t_justifyRight k c = justifyRight j c `eqP` (unpackS . T.justifyRight j c)
    where j = fromIntegral (k :: Word8)
tl_justifyRight k c = justifyRight j c `eqP`
                      (unpackS . TL.justifyRight (fromIntegral j) c)
    where j = fromIntegral (k :: Word8)
t_center k c = center j c `eqP` (unpackS . T.center j c)
    where j = fromIntegral (k :: Word8)
tl_center k c = center j c `eqP` (unpackS . TL.center (fromIntegral j) c)
    where j = fromIntegral (k :: Word8)

t_elem c          = L.elem c `eqP` T.elem c
tl_elem c         = L.elem c `eqP` TL.elem c
sf_elem (applyFun -> p) c = (L.elem c . L.filter p) `eqP` (S.elem c . S.filter p)
sf_filter (applyFun -> q) (applyFun -> p)
                  = (L.filter p . L.filter q) `eqP` (unpackS . S.filter p . S.filter q)

t_filter (applyFun -> p)
                  = L.filter p    `eqP` (unpackS . T.filter p)
tl_filter (applyFun -> p)
                  = L.filter p    `eqP` (unpackS . TL.filter p)
t_filter_filter (applyFun -> p) (applyFun -> q)
                  = (L.filter p . L.filter q) `eqP` (unpackS . T.filter p . T.filter q)
tl_filter_filter (applyFun -> p) (applyFun -> q)
                  = (L.filter p . L.filter q) `eqP` (unpackS . TL.filter p . TL.filter q)
t_length_filter (applyFun -> p)
                  = (L.length . L.filter p) `eqP` (T.length . T.filter p)
tl_length_filter (applyFun -> p)
                  = (L.genericLength . L.filter p) `eqP` (TL.length . TL.filter p)

sf_findBy (applyFun -> q) (applyFun -> p)
                             = (L.find p . L.filter q) `eqP` (S.findBy p . S.filter q)
t_find (applyFun -> p)       = L.find p      `eqP` T.find p
tl_find (applyFun -> p)      = L.find p      `eqP` TL.find p
t_partition (applyFun -> p)  = L.partition p `eqP` (unpack2 . T.partition p)
tl_partition (applyFun -> p) = L.partition p `eqP` (unpack2 . TL.partition p)

sf_index (applyFun -> p) s i = ((L.filter p s L.!!) `eq` S.index (S.filter p $ packS s)) j
    where l = L.length s
          j = if l == 0 then 0 else i `mod` (3 * l) - l
t_index s i       = ((s L.!!) `eq` T.index (packS s)) j
    where l = L.length s
          j = if l == 0 then 0 else i `mod` (3 * l) - l

tl_index s i      = ((s L.!!) `eq` (TL.index (packS s) . fromIntegral)) j
    where l = L.length s
          j = if l == 0 then 0 else i `mod` (3 * l) - l

t_findIndex (applyFun -> p) = L.findIndex p `eqP` T.findIndex p
t_count (NotEmpty t)  = (subtract 1 . L.length . T.splitOn t) `eq` T.count t
tl_count (NotEmpty t) = (subtract 1 . L.genericLength . TL.splitOn t) `eq`
                        TL.count t
t_zip s           = L.zip s `eqP` T.zip (packS s)
tl_zip s          = L.zip s `eqP` TL.zip (packS s)
sf_zipWith (applyFun -> p) (applyFun2 -> c) s
                  = (L.zipWith c (L.filter p s) . L.filter p) `eqP`
                    (unpackS . S.zipWith c (S.filter p $ packS s) . S.filter p)
t_zipWith (applyFun2 -> c) s         = L.zipWith c s `eqP` (unpackS . T.zipWith c (packS s))
tl_zipWith (applyFun2 -> c) s        = L.zipWith c s `eqP` (unpackS . TL.zipWith c (packS s))
t_length_zipWith (applyFun2 -> c) s  = (L.length . L.zipWith c s) `eqP` (T.length . T.zipWith c (packS s))
tl_length_zipWith (applyFun2 -> c) s = (L.genericLength . L.zipWith c s) `eqP` (TL.length . TL.zipWith c (packS s))

t_indices  (NotEmpty s) = Slow.indices s `eq` T.indices s
tl_indices (NotEmpty s) = lazyIndices s `eq` S.indices s
    where lazyIndices ss t = map fromIntegral $ Slow.indices (conc ss) (conc t)
          conc = T.concat . TL.toChunks
t_indices_occurs = \(Sqrt (NotEmpty t)) ts ->
    let s = T.intercalate t ts
    in Slow.indices t s === T.indices t s

t_indices_drop5 = T.indices (T.pack "no") (T.drop 5 (T.pack "abcdefghijklmno")) === [8]
tl_indices_drop5 = S.indices (TL.pack "no") (TL.drop 5 (TL.pack "abcdefghijklmno")) === [8]

t_indices_drop n s pref suff = T.indices s t === Slow.indices s t
  where
    t = T.drop n $ pref `T.append` s `T.append` suff
tl_indices_drop n s pref suff =
  map fromIntegral (S.indices s t) === Slow.indices (TL.toStrict s) (TL.toStrict t)
  where
    t = TL.drop n $ pref `TL.append` s `TL.append` suff

tl_indices_chunked = S.indices (TL.pack "1234") (TL.pack "1" `TL.append` TL.pack "234" `TL.append` TL.pack "567") === [0]
tl_indices_drop_chunked n s pref suff =
  map fromIntegral (S.indices s t) === Slow.indices (TL.toStrict s) (TL.toStrict t)
  where
    -- constructing a pathologically chunked haystack
    t = TL.concatMap TL.singleton $ TL.drop n $ pref `TL.append` s `TL.append` suff

t_indices_char_drop n c pref suff = T.indices s t === Slow.indices s t
  where
    s = T.singleton c
    t = T.drop n $ pref `T.append` s `T.append` suff
tl_indices_char_drop n c pref suff = map fromIntegral (S.indices s t) === Slow.indices (TL.toStrict s) (TL.toStrict t)
  where
    s = TL.singleton c
    t = TL.drop n $ pref `TL.append` s `TL.append` suff

-- Make a stream appear shorter than it really is, to ensure that
-- functions that consume inaccurately sized streams behave
-- themselves.
shorten :: Int -> S.Stream a -> S.Stream a
shorten n t@(S.Stream arr off len)
    | n > 0     = S.Stream arr off (smaller (exactSize n) len)
    | otherwise = t

testText :: TestTree
testText =
  testGroup "Text" [
    testGroup "creation/elimination" [
      testProperty "t_pack_unpack" t_pack_unpack,
      testProperty "tl_pack_unpack" tl_pack_unpack,
      testProperty "t_stream_unstream" t_stream_unstream,
      testProperty "tl_stream_unstream" tl_stream_unstream,
      testProperty "t_reverse_stream" t_reverse_stream,
      testProperty "t_singleton" t_singleton,
      testProperty "tl_singleton" tl_singleton,
      testProperty "tl_unstreamChunks" tl_unstreamChunks,
      testProperty "tl_chunk_unchunk" tl_chunk_unchunk,
      testProperty "tl_from_to_strict" tl_from_to_strict
    ],

    testGroup "transformations" [
      testProperty "s_map" s_map,
      testProperty "s_map_s" s_map_s,
      testProperty "sf_map" sf_map,

      testProperty "t_map" t_map,
      testProperty "tl_map" tl_map,
      testProperty "t_map_map" t_map_map,
      testProperty "tl_map_map" tl_map_map,
      testProperty "t_length_map" t_length_map,
      testProperty "tl_length_map" tl_length_map,

      testProperty "s_intercalate" s_intercalate,
      testProperty "t_intercalate" t_intercalate,
      testProperty "tl_intercalate" tl_intercalate,
      testProperty "t_length_intercalate" t_length_intercalate,
      testProperty "tl_length_intercalate" tl_length_intercalate,
      testProperty "s_intersperse" s_intersperse,
      testProperty "s_intersperse_s" s_intersperse_s,
      testProperty "sf_intersperse" sf_intersperse,
      testProperty "t_intersperse" t_intersperse,
      testProperty "tl_intersperse" tl_intersperse,
      testProperty "t_length_intersperse" t_length_intersperse,
      testProperty "tl_length_intersperse" tl_length_intersperse,
      testProperty "t_transpose" t_transpose,
      testProperty "tl_transpose" tl_transpose,
      testProperty "t_reverse" t_reverse,
      testProperty "tl_reverse" tl_reverse,
      testProperty "t_reverse_short" t_reverse_short,
      testProperty "t_replace" t_replace,
      testProperty "tl_replace" tl_replace,

      testGroup "case conversion" [
        testProperty "s_toCaseFold_length" s_toCaseFold_length,
        testProperty "sf_toCaseFold_length" sf_toCaseFold_length,
        testProperty "t_toCaseFold_length" t_toCaseFold_length,
        testProperty "tl_toCaseFold_length" tl_toCaseFold_length,
#if MIN_VERSION_base(4,16,0)
        testProperty "t_toCaseFold_char" t_toCaseFold_char,
#endif
        testProperty "t_toCaseFold_exceptions" t_toCaseFold_exceptions,
        testProperty "t_toCaseFold_cherokeeLower" t_toCaseFold_cherokeeLower,
        testProperty "t_toCaseFold_cherokeeUpper" t_toCaseFold_cherokeeUpper,

        testProperty "t_toLower_length" t_toLower_length,
        testProperty "t_toLower_lower" t_toLower_lower,
        testProperty "tl_toLower_lower" tl_toLower_lower,
        testProperty "t_toLower_dotted_i" t_toLower_dotted_i,

        testProperty "t_toUpper_length" t_toUpper_length,
        testProperty "t_toUpper_upper" t_toUpper_upper,
        testProperty "tl_toUpper_upper" tl_toUpper_upper,
        testProperty "t_toUpper_exceptions" t_toUpper_exceptions,

        testProperty "t_toTitle_title" t_toTitle_title,
        testProperty "t_toTitle_1stNotLower" t_toTitle_1stNotLower,
        testProperty "t_toTitle_exceptions" t_toTitle_exceptions,

#if MIN_VERSION_base(4,13,0)
        -- Requires base compliant with Unicode 12.0
        testProperty "t_toLower_char" t_toLower_char,
        testProperty "t_toUpper_char" t_toUpper_char,
        testProperty "t_toTitle_char" t_toTitle_char,
#endif

        testProperty "t_toUpper_idempotent" t_toUpper_idempotent,
        testProperty "t_toLower_idempotent" t_toLower_idempotent,
        testProperty "t_toCaseFold_idempotent" t_toCaseFold_idempotent,

        testProperty "ascii_toLower" ascii_toLower,
        testProperty "ascii_toUpper" ascii_toUpper,
        testProperty "ascii_toTitle" ascii_toTitle,
        testProperty "ascii_toCaseFold" ascii_toCaseFold
      ],

      testGroup "justification" [
        testProperty "s_justifyLeft" s_justifyLeft,
        testProperty "s_justifyLeft_s" s_justifyLeft_s,
        testProperty "sf_justifyLeft" sf_justifyLeft,
        testProperty "t_justifyLeft" t_justifyLeft,
        testProperty "tl_justifyLeft" tl_justifyLeft,
        testProperty "t_justifyRight" t_justifyRight,
        testProperty "tl_justifyRight" tl_justifyRight,
        testProperty "t_center" t_center,
        testProperty "tl_center" tl_center
      ]
    ],

    testGroup "searching" [
      testProperty "t_elem" t_elem,
      testProperty "tl_elem" tl_elem,
      testProperty "sf_elem" sf_elem,
      testProperty "sf_filter" sf_filter,
      testProperty "t_filter" t_filter,
      testProperty "tl_filter" tl_filter,
      testProperty "t_filter_filter" t_filter_filter,
      testProperty "tl_filter_filter" tl_filter_filter,
      testProperty "t_length_filter" t_length_filter,
      testProperty "tl_length_filter" tl_length_filter,
      testProperty "sf_findBy" sf_findBy,
      testProperty "t_find" t_find,
      testProperty "tl_find" tl_find,
      testProperty "t_partition" t_partition,
      testProperty "tl_partition" tl_partition
    ],

    testGroup "indexing" [
      testProperty "sf_index" sf_index,
      testProperty "t_index" t_index,
      testProperty "tl_index" tl_index,
      testProperty "t_findIndex" t_findIndex,
      testProperty "t_count" t_count,
      testProperty "tl_count" tl_count,
      testProperty "t_indices" t_indices,
      testProperty "tl_indices" tl_indices,
      testProperty "t_indices_occurs" t_indices_occurs,

      testProperty "t_indices_drop5" t_indices_drop5,
      testProperty "tl_indices_drop5" tl_indices_drop5,
      testProperty "t_indices_drop" t_indices_drop,
      testProperty "tl_indices_drop" tl_indices_drop,
      testProperty "tl_indices_chunked" tl_indices_chunked,
      testProperty "tl_indices_drop_chunked" tl_indices_drop_chunked,
      testProperty "t_indices_char_drop" t_indices_char_drop,
      testProperty "tl_indices_char_drop" tl_indices_char_drop
    ],

    testGroup "zips" [
      testProperty "t_zip" t_zip,
      testProperty "tl_zip" tl_zip,
      testProperty "sf_zipWith" sf_zipWith,
      testProperty "t_zipWith" t_zipWith,
      testProperty "tl_zipWith" tl_zipWith,
      testProperty "t_length_zipWith" t_length_zipWith,
      testProperty "tl_length_zipWith" tl_length_zipWith
    ]
  ]
