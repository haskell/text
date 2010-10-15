-- Contributed by Ken Friis Larsen and Morten Ib Nielsen.

{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Environment (getArgs)
import qualified Char

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE


import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE

replaceTagsM file tag sub = 
  BC.readFile file >>= BC.putStr . replaceTags tag sub . TE.encodeUtf8 . T.toLower . TE.decodeUtf8 
  where 
    replaceTags tag replacement str = B.concat $ reverse $ replaceTags' [] (BC.pack $ '<' : tag) '>' (BC.pack replacement) str
    replaceTags' !res start end repl str =
      let (pre, post) = BC.breakSubstring start str
      in if BC.null post
           then  pre : res
           else replaceTags' (repl : pre : res) start end repl $ BC.drop 1 $
                BC.dropWhile (/= end) post

splitB sep str = seplen `seq` splitter str 
  where 
    splitter str = h : if B.null t then [] else splitter (B.drop seplen t)
      where (h,t) = B.breakSubstring sep str
    seplen = B.length sep
    
replaceTagsWrong file tagName sub = do
  content <- BC.readFile file
  let frags = map (BC.drop 1 . BC.dropWhile (/= '>')) 
              $ splitB (BC.pack $ '<' : tagName) (BC.map Char.toLower content)
  BC.putStr $ BC.intercalate (BC.pack sub) frags
 
replaceTagsK file tagName sub = do
  raw <- BC.readFile file 
  let content = (TE.encodeUtf8 . T.toLower . TE.decodeUtf8) raw
  let frags = map (BC.drop 1 . BC.dropWhile (/= '>')) 
              $ splitB (BC.pack $ '<' : tagName) content
  BC.putStr $ BC.intercalate (BC.pack sub) frags

replaceTagsO file tagName sub = do
  raw <- BC.readFile file 
  let content = (TE.encodeUtf8 . T.toLower . TE.decodeUtf8) raw
  let frags = splitB (BC.pack $ '<' : tagName) content
  BC.putStr $ BC.intercalate (BC.pack sub) frags
  where 
    splitB sep str = splitter str 
      where 
        splitter str = h : if BC.null t then [] else splitter (BC.drop 1 $ BC.dropWhile (/= '>') t)
          where (h,t) = B.breakSubstring sep str


    
replaceTagsT file tagName sub = do
  raw <- B.readFile file 
  let content = TE.decodeUtf8 raw
  let frags = map (T.drop 1 . T.dropWhile (/= '>')) 
              $ T.split (T.pack $ '<' : tagName) (T.toLower content)
  T.putStr $ T.intercalate (T.pack sub) frags
  
replaceTagsTL file tagName sub = do
  raw <- BL.readFile file 
  let content = TLE.decodeUtf8 raw
  let frags = map (TL.drop 1 . TL.dropWhile (/= '>')) 
              $ TL.split (TL.pack $ '<' : tagName) (TL.toLower content)
  TL.putStr $ TL.intercalate (TL.pack sub) frags


main = do
  (kind : file : tag : sub : _) <- getArgs
  case kind of
    "Text" -> replaceTagsT file tag sub
    "TextLazy" -> replaceTagsTL file tag sub
    "BytestringM" -> replaceTagsM file tag sub
    "BytestringK" -> replaceTagsK file tag sub
    "BytestringO" -> replaceTagsO file tag sub
    "TextNull" -> T.readFile file >>= T.putStr
    "ByteNull" -> B.readFile file >>= B.putStr
    "EncodeNull" -> B.readFile file >>= T.putStr . T.toLower . TE.decodeUtf8 

