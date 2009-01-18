import Prelude hiding (zip,zip3,fst,snd)

import BenchUtils
import Data.Char
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Fusion (Encoding(..))
import qualified Data.Text.Fusion as S
import Text.Printf
import System.IO
import System.Mem
import qualified System.IO.UTF8 as UTF8

main = do ascii_str <- readFile "ascii.txt" 
          ascii_bs  <- B.readFile "ascii.txt"
          let ascii_txt = T.decode ASCII ascii_bs
          force (ascii_txt,ascii_str,ascii_bs)
          printf " # Text\t\tString\tByteString\n"
          run 1 (ascii_txt,ascii_str,ascii_bs) ascii_tests

ascii_tests =  [
 ("map/map",
  [F $ T.map pred . T.map succ . fst, 
   Flist $ L.map pred . L.map succ . snd,
   F $ B.map pred . B.map succ . trd]),
 ("filter/filter",
  [F $ T.filter (/= '\101') . T.filter (/= '\102') . fst,
   Flist $ L.filter (/= '\101') . L.filter (/= '\102') . snd,
   F $ B.filter (/= 101) . B.filter (/= 102) . trd]),
 ("filter/map",
  [F $ T.filter (/= '\103') . T.map succ . fst,
   Flist $ L.filter (/= '\103') . L.map succ . snd,
   F $ B.filter (/= 103) . B.map succ . trd]),
 ("map/filter",
  [F $ T.map succ . T.filter (/= '\104') . fst,
   Flist $ L.map succ . L.filter (/= '\104') . snd,
   F $ B.map succ . B.filter (/= 104) . trd]),
 ("foldl'/map",
  [F $ T.foldl' (const . (+1)) (0 :: Int) . T.map succ . fst,
   F $ L.foldl' (const . (+1)) (0 :: Int) . L.map succ . snd,
   F $ B.foldl' (const . (+1)) (0 :: Int) . B.map succ . trd]),
 ("foldl'/filter",
  [F $ T.foldl' (const . (+2)) (0::Int) . T.filter (/= '\105') . fst,
   F $ L.foldl' (const . (+2)) (0::Int) . L.filter (/= '\105') . snd,
   F $ B.foldl' (const . (+2)) (0::Int) . B.filter (/= 105) . trd]),
 ("foldl'/map/filter",
  [F $ T.foldl' (const.(+3)) (0::Int) . T.map succ . T.filter (/='\110') . fst,
   F $ L.foldl' (const.(+3)) (0::Int) . L.map succ . L.filter (/='\110') . snd,
   F $ B.foldl' (const . (+3)) (0::Int) . B.map succ . B.filter (/= 110) . trd])
 ]
