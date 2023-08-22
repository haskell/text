{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Tests.ShareEmpty
  ( tests
  ) where

import Control.Exception (evaluate)
import Data.Text
import Language.Haskell.TH.Syntax (lift)
import Test.Tasty.HUnit (testCase, assertFailure, assertEqual)
import Test.Tasty (TestTree, testGroup)
import GHC.Exts
import GHC.Stack
import qualified Data.List as L
import qualified Data.Text as T


-- | assert that a text value is represented by the same pointer
-- as the 'empty' value.
assertPtrEqEmpty :: HasCallStack => Text -> IO ()
assertPtrEqEmpty t = do 
    t' <- evaluate t
    empty' <- evaluate empty
    assertEqual "" empty' t'
    case reallyUnsafePtrEquality# empty' t' of
      1# -> pure ()
      _ -> assertFailure "Pointers are not equal"
{-# NOINLINE assertPtrEqEmpty #-}

tests :: TestTree
tests = testGroup "empty Text values are shared"
  [ testCase "empty = empty" $ assertPtrEqEmpty T.empty
  , testCase "pack \"\" = empty" $ assertPtrEqEmpty $ T.pack ""
  , testCase "fromString \"\" = empty" $ assertPtrEqEmpty $ fromString ""
  , testCase "$(lift \"\") = empty" $ assertPtrEqEmpty $ $(lift (pack ""))
  , testCase "tail of a singleton = empty" $ assertPtrEqEmpty $ T.tail "a"
  , testCase "init of a singleton = empty" $ assertPtrEqEmpty $ T.init "b"
  , testCase "map _ empty = empty" $ assertPtrEqEmpty $ T.map id empty
  , testCase "intercalate _ [] = empty" $ assertPtrEqEmpty $ T.intercalate ", " []
  , testCase "intersperse _ empty = empty" $ assertPtrEqEmpty $ T.intersperse ',' ""
  , testCase "reverse empty = empty" $ assertPtrEqEmpty $
      T.reverse empty
  , testCase "replace _ _ empty = empty" $ assertPtrEqEmpty $
      T.replace "needle" "replacement" empty
  , testCase "toCaseFold empty = empty" $ assertPtrEqEmpty $ T.toCaseFold ""
  , testCase "toLower empty = empty" $ assertPtrEqEmpty $ T.toLower ""
  , testCase "toUpper empty = empty" $ assertPtrEqEmpty $ T.toUpper ""
  , testCase "toTitle empty = empty" $ assertPtrEqEmpty $ T.toTitle ""
  , testCase "justifyLeft 0 _ empty = empty" $ assertPtrEqEmpty $
      justifyLeft 0 ' ' empty
  , testCase "justifyRight 0 _ empty = empty" $ assertPtrEqEmpty $
      justifyRight 0 ' ' empty
  , testCase "center 0 _ empty = empty" $ assertPtrEqEmpty $
      T.center 0 ' ' empty
  , testCase "transpose [empty] = [empty]" $ mapM_ assertPtrEqEmpty $
      T.transpose [empty]
  , testCase "concat [] = empty" $ assertPtrEqEmpty $ T.concat []
  , testCase "concat [empty] = empty" $ assertPtrEqEmpty $ T.concat [empty]
  , testCase "replicate 0 _ = empty" $ assertPtrEqEmpty $ T.replicate 0 "x"
  , testCase "replicate _ empty = empty" $ assertPtrEqEmpty $ T.replicate 10 empty
  , testCase "unfoldr (const Nothing) _ = empty" $ assertPtrEqEmpty $
      T.unfoldr (const Nothing) ()
  , testCase "take 0 _ = empty" $ assertPtrEqEmpty $
      T.take 0 "xyz"
  , testCase "takeEnd 0 _ = empty" $ assertPtrEqEmpty $
      T.takeEnd 0 "xyz"
  , testCase "takeWhile (const False) _ = empty" $ assertPtrEqEmpty $
      T.takeWhile (const False) "xyz"
  , testCase "takeWhileEnd (const False) _ = empty" $ assertPtrEqEmpty $
      T.takeWhileEnd (const False) "xyz"
  , testCase "drop n x = empty where n > len x" $ assertPtrEqEmpty $
      T.drop 5 "xyz"
  , testCase "dropEnd n x = empty where n > len x" $ assertPtrEqEmpty $
      T.dropEnd 5 "xyz"
  , testCase "dropWhile (const True) x = empty" $ assertPtrEqEmpty $
      T.dropWhile (const True) "xyz"
  , testCase "dropWhileEnd (const True) x = empty" $ assertPtrEqEmpty $
      dropWhileEnd (const True) "xyz"
  , testCase "dropAround _ empty = empty" $ assertPtrEqEmpty $
      dropAround (const True) empty
  , testCase "stripStart empty = empty" $ assertPtrEqEmpty $ T.stripStart empty
  , testCase "stripEnd empty = empty" $ assertPtrEqEmpty $ T.stripEnd empty
  , testCase "strip empty = empty" $ assertPtrEqEmpty $ T.strip empty
  , testCase "fst (splitAt 0 _) = empty" $ assertPtrEqEmpty $ fst $ T.splitAt 0 "123"
  , testCase "snd (splitAt n x) = empty where n > len x" $ assertPtrEqEmpty $
      snd $ T.splitAt 5 "123"
  , testCase "fst (span (const False) _) = empty" $ assertPtrEqEmpty $
      fst $ T.span (const False) "123"
  , testCase "snd (span (const True) _) = empty" $ assertPtrEqEmpty $
      snd $ T.span (const True) "123"
  , testCase "fst (break (const False) _) = empty" $ assertPtrEqEmpty $
      fst $ T.span (const False) "123"
  , testCase "snd (break (const True) _) = empty" $ assertPtrEqEmpty $
      snd $ T.span (const True) "123"
  , testCase "fst (spanM (const $ pure False) _) = empty" $
      assertPtrEqEmpty . fst =<< T.spanM (const $ pure False) "123"
  , testCase "snd (spanM (const $ pure True) _) = empty" $
      assertPtrEqEmpty . snd =<< T.spanM (const $ pure True) "123"
  , testCase "fst (spanEndM (const $ pure True) _) = empty" $
      assertPtrEqEmpty . fst =<< T.spanEndM (const $ pure True) "123"
  , testCase "snd (spanEndM (const $ pure False) _) = empty" $
      assertPtrEqEmpty . snd =<< T.spanEndM (const $ pure False) "123"
  , testCase "groupBy _ empty = [empty]" $ mapM_ assertPtrEqEmpty $ T.groupBy (==) empty
  , testCase "inits empty = [empty]" $ mapM_ assertPtrEqEmpty $ T.inits empty
  , testCase "inits _ = [empty, ...]" $ assertPtrEqEmpty $ L.head $ T.inits "123"
  , testCase "tails empty = [empty]" $ mapM_ assertPtrEqEmpty $ T.tails empty
  , testCase "tails _ = [..., empty]" $ assertPtrEqEmpty $ L.last $ T.tails "123"
  , testCase "tails _ = [..., empty]" $ assertPtrEqEmpty $ L.last $ T.tails "123"
  , testCase "split _ empty = [empty]" $ mapM_ assertPtrEqEmpty $ T.split (== 'a') ""
  , testCase "filter (const False) _ = empty" $ assertPtrEqEmpty $ T.filter (const False) "1234"
  , testCase "zipWith const empty empty = empty" $ assertPtrEqEmpty $ T.zipWith const "" ""
  , testCase "unlines [] = empty" $ assertPtrEqEmpty $ T.unlines []
  , testCase "unwords [] = empty" $ assertPtrEqEmpty $ T.unwords []
  , testCase "stripPrefix empty empty = Just empty" $ mapM_ assertPtrEqEmpty $
      T.stripPrefix empty empty
  , testCase "stripSuffix empty empty = Just empty" $ mapM_ assertPtrEqEmpty $
      T.stripSuffix empty empty
  , testCase "commonPrefixes \"xyz\" \"123\" = Just (_, empty, _)" $
      mapM_ (assertPtrEqEmpty . (\(_, x, _) -> x)) $ T.commonPrefixes "xyz" "123"
  , testCase "commonPrefixes \"xyz\" \"xyz\" = Just (_, _, empty)" $
      mapM_ (assertPtrEqEmpty . (\(_, _, x) -> x)) $ T.commonPrefixes "xyz" "xyz"
  , testCase "copy empty = empty" $ assertPtrEqEmpty $ T.copy ""
  ]
