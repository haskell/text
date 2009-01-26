--module Bench where

import BenchUtils
import System.Mem 
import Control.Concurrent
import Data.Char
import Data.Array.IArray
import System.CPUTime
import System.IO
import System.IO.Unsafe
import Text.Printf
import Control.Exception

import qualified Data.Text as T
import Data.Text.Internal
import qualified Data.Text.Fusion as S
import Data.Text.Fusion (Encoding(..))

import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as B8
import Data.ByteString (ByteString)
import Data.Word
import qualified System.IO.UTF8 as UTF8

main = do ascii_bs <- B.readFile "text/test/ascii.txt"
          let ascii_txt = T.decode ASCII ascii_bs 
          let ascii_str = T.unpack ascii_txt
          force (ascii_txt,ascii_str,ascii_bs)
          printf " # Text\t\tString\tByteString\n"
          run 1 (ascii_txt,ascii_str,ascii_bs) ascii_tests
          
          
ascii_tests = [
                ("cons",
                 [F (app1 (T.cons '\88')),
                  F (app2 ((:) '\88')    ),
                  F (app3 (B8.cons 88) )]),
                ("head",
                 [F (app1 T.head), 
                  F (app2 L.head),
                  F (app3 B.head)]),
                ("last",
                 [F (app1 T.last),
                  F (app2 L.last),
                  F (app3 B.last)]),
                ("tail",
                 [F (app1 T.tail),
                  F (app2 L.tail),
                  F (app3 B.tail)]),
                ("init",
                 [F (app1 T.init),
                  Flist (app2 L.init),
                  F (app3 B.init) ]),
                ("null",
                 [F (app1 T.null),
                  F (app2 L.null),
                  F (app3 B.null) ]),
                ("length",
                 [F (app1 T.length),
                  F (app2 L.length),
                  F (app3 B.length)]),
                 ("map",
                  [F (app1 $ T.map succ), 
                   Flist (app2 (L.map succ)),
                   F (app3 $ B.map succ)]),
                 ("filter",
                  [F $ app1 $ T.filter (/= '\101'),
                   Flist $ app2 $ L.filter (/= '\101'),
                   F $ app3 $ B8.filter (/= 101)]),
                 ("foldl'",
                  [F (app1 $ T.foldl' (\a w -> a+1::Int) 0),
                   F (app2 $ L.foldl' (\a w -> a+1::Int) 0),
                   F (app3 $ B.foldl' (\a w -> a+1::Int) 0)
                  ]),
                 ("drop",
                  [F (app1 $ T.drop 30000000),
                   Flist (app2 $ L.drop 30000000),
                   F (app3 $ B.drop 30000000)
                  ]),
                 ("take",
                  [F (app1 $ T.take 30000000),
                   Flist (app2 $ L.take 30000000),
                   F (app3 $ B.take 30000000)]),
                 ("words",
                  [F (app1 $ T.words),
                   Flist (app2 $ L.words),
                   F (app3 $ B.words)])
 ]

bmp_tests = [
                ("cons",
                 [F (app1 (T.cons '\88')),
                  F (app2 ((:) '\88')    )]),
                ("head",
                 [F (app1 T.head), 
                  F (app2 L.head)]),
                ("last",
                 [F (app1 T.last),
                  F (app2 L.last)]),
                ("tail",
                 [F (app1 T.tail),
                  F (app2 L.tail)]),
                ("init",
                 [F (app1 T.init),
                  Flist (app2 L.init)]),
                ("null",
                 [F (app1 T.null),
                  F (app2 L.null),
                  F (app3 B.null)]),
                  ("length",
                   [F (app1 T.length),
                    F (app2 L.length),
                    F (app3 B.length)]),
                 ("map",
                  [F (app1 $ T.map succ), 
                   Flist (app2 (L.map succ))]),
                 ("filter",
                  [F $ app1 $ T.filter (/= '\101'),
                   Flist $ app2 $ L.filter (/= '\101')]),
                 ("foldl'",
                  [F (app1 $ T.foldl' (\a w -> a+1::Int) 0),
                   F (app2 $ L.foldl' (\a w -> a+1::Int) 0)]),
                 ("drop",
                  [F (app1 $ T.drop 30000000),
                   Flist (app2 $ L.drop 30000000)]),
                 ("take",
                  [F (app1 $ T.take 30000000),
                   Flist (app2 $ L.take 30000000)]),
                 ("words",
                  [F (app1 $ T.words),
                   Flist (app2 $ L.words)])
 ]

smp_sip_tests = [
                ("cons",
                 [F (app1 (T.cons '\65624')),
                  F (app2 ((:) '\65624'))]),
                ("head",
                 [F (app1 T.head), 
                  F (app2 L.head)]),
                ("last",
                 [F (app1 T.last),
                  F (app2 L.last)]),
                ("tail",
                 [F (app1 T.tail),
                  F (app2 L.tail)]),
                ("init",
                 [F (app1 T.init),
                  Flist (app2 L.init)]),
                ("null",
                 [F (app1 T.null),
                  F (app2 L.null),
                  F (app3 B.null) ]),
                ("length",
                 [F (app1 T.length ),
                  F (app2 L.length),
                  F (app3 B.length)]),
                 ("map",
                  [F (app1 $ T.map succ), 
                   Flist (app2 (L.map succ))]),
                 ("filter",
                  [F $ app1 $ T.filter (/= '\65624'),
                   Flist $ app2 $ L.filter (/= '\65624')]),
                 ("foldl'",
                  [F (app1 $ T.foldl' (\a w -> a+1::Int) 0),
                   F (app2 $ L.foldl' (\a w -> a+1::Int) 0)]),
                 ("drop",
                  [F (app1 $ T.drop 30000000),
                   Flist (app2 $ L.drop 30000000)]),
                 ("take",
                  [F (app1 $ T.take 30000000),
                   Flist (app2 $ L.take 30000000)])
                ]
