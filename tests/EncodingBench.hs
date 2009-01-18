{-# OPTIONS_GHC -fglasgow-exts #-}

import BenchUtils
import qualified Data.Text.Utf8.Fusion as U8
import qualified Data.Text.Utf8.Internal as U8I
import qualified Data.Text.Utf32.Fusion as U32
import qualified Data.Text.Utf32.Internal as U32I
import qualified Data.Text.Fusion as S
import Data.Text.Fusion (bsStream,Encoding(..))
import qualified Data.Text as T
import qualified Data.ByteString as B
import Text.Printf
import System.Mem

instance Forceable U32I.Text
instance Forceable U8I.Text

data E a = forall b. E (a -> b) | EText (a -> S.Stream Char)

main = do force (ascii_tests, smp_sip_tests)
          ascii       <- B.readFile "ascii.txt"
          let ascii8  = U8.unstream  (bsStream ascii ASCII)
          let ascii16 = S.unstream   (bsStream ascii ASCII)
          let ascii32 = U32.unstream (bsStream ascii ASCII)
          force (ascii8, ascii16, ascii32)
          printf " # Utf8\t\tUtf16\tUtf32\n"
          run 1 (ascii8, ascii16, ascii32) ascii_tests
          performGC
          bmp   <- B.readFile "bmp.txt"
          let bmp8  = U8.unstream  (bsStream bmp Utf8)
          let bmp16 = S.unstream   (bsStream bmp Utf8)
          let bmp32 = U32.unstream (bsStream bmp Utf8)
          force (bmp8, bmp16, bmp32)
          printf " # Utf8\t\tUtf16\tUtf32\n"
          run 1 (bmp8, bmp16, bmp32)     ascii_tests
          performGC
          smp_sip       <- B.readFile "smp_sip.txt"
          let smp_sip8  = U8.unstream (bsStream smp_sip Utf8)
          let smp_sip16 = S.unstream (bsStream smp_sip Utf8)
          let smp_sip32 = U32.unstream (bsStream smp_sip Utf8)
          force (smp_sip8, smp_sip16, smp_sip32)
          printf " # Utf8\t\tUtf16\tUtf32\n"
          run 1 (smp_sip8, smp_sip16, smp_sip32) smp_sip_tests
          
ascii_tests = [
    ("cons"  , 
     [F $ app1 $ U8.unstream  . S.cons '\88' . U8.stream, 
      F $ app2 $ S.unstream   . S.cons '\88' . S.stream, 
      F $ app3 $ U32.unstream . S.cons '\88' . U32.stream]),
    ("length", 
     [F $ app1 $ S.length . U8.stream,
      F $ app2 $ S.length . S.stream,
      F $ app3 $ S.length . U32.stream]),
    ("map"   , 
     [F $ app1 $ U8.unstream  . S.map succ . U8.stream,
      F $ app2 $ S.unstream   . S.map succ . S.stream, 
      F $ app3 $ U32.unstream . S.map succ . U32.stream]),
    ("filter", 
     [F $ app1 $ U8.unstream  . S.filter (/= '\101') . U8.stream,
      F $ app2 $ S.unstream   . S.filter (/= '\101') . S.stream,
      F $ app3 $ U32.unstream . S.filter (/= '\101') . U32.stream]),
    ("take", 
     [F $ app1 $ U8.unstream  . S.take 1000000 . U8.stream,
      F $ app2 $ S.unstream   . S.take 1000000 . S.stream,
      F $ app3 $ U32.unstream . S.take 1000000 . U32.stream]),
    ("drop"  , 
     [F $ app1 $ U8.unstream  . S.drop 1000000 . U8.stream,
      F $ app2 $ S.unstream   . S.drop 1000000 . S.stream,
      F $ app3 $ U32.unstream . S.drop 1000000 . U32.stream]),
    ("foldl'",
     [F $ app1 $ S.foldl' (\a w -> a+1::Int) 0 . U8.stream,
      F $ app2 $ S.foldl' (\a w -> a+1::Int) 0 . S.stream,
      F $ app3 $ S.foldl' (\a w -> a+1::Int) 0 . U32.stream
     ])
 ]

smp_sip_tests = [
    ("cons"  , 
     [F $ app1 $ U8.unstream  . S.cons '\88' . U8.stream, 
      F $ app2 $ S.unstream   . S.cons '\88' . S.stream, 
      F $ app3 $ U32.unstream . S.cons '\88' . U32.stream]),
    ("length", 
     [F $ app1 $ S.length . U8.stream,
      F $ app2 $ S.length . S.stream,
      F $ app3 $ S.length . U32.stream]),
    ("map"   , 
     [F $ app1 $ U8.unstream  . S.map succ . U8.stream,
      F $ app2 $ S.unstream   . S.map succ . S.stream, 
      F $ app3 $ U32.unstream . S.map succ . U32.stream]),
    ("filter", 
     [F $ app1 $ U8.unstream  . S.filter (/= '\101') . U8.stream,
      F $ app2 $ S.unstream   . S.filter (/= '\101') . S.stream,
      F $ app3 $ U32.unstream . S.filter (/= '\101') . U32.stream]),
    ("take", 
     [F $ app1 $ U8.unstream  . S.take 1000000 . U8.stream,
      F $ app2 $ S.unstream   . S.take 1000000 . S.stream,
      F $ app3 $ U32.unstream . S.take 1000000 . U32.stream]),
    ("drop"  , 
     [F $ app1 $ U8.unstream  . S.drop 1000000 . U8.stream,
      F $ app2 $ S.unstream   . S.drop 1000000 . S.stream,
      F $ app3 $ U32.unstream . S.drop 1000000 . U32.stream]),
    ("foldl'",
     [F $ app1 $ S.foldl' (\a w -> a+1::Int) 0 . U8.stream,
      F $ app2 $ S.foldl' (\a w -> a+1::Int) 0 . S.stream,
      F $ app3 $ S.foldl' (\a w -> a+1::Int) 0 . U32.stream])
 ]