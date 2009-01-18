{-# LANGUAGE ExistentialQuantification #-}
module BenchUtils where

import qualified Data.List as L
import Data.ByteString (ByteString(..))
import Data.Word
import Text.Printf
import System.IO
import Data.Text.Internal (Text(..))
import System.Mem
import System.CPUTime
import Control.Exception
import Control.Concurrent

data Result = T | B

data F a = forall b. F (a -> b) | forall b. Flist (a -> [b])

class Forceable a where
    force :: a -> IO Result
    force v = v `seq` return T

instance Forceable Text 

seqList = L.foldl' (flip seq) (return ())
instance Forceable [a] where
    force = L.foldl' (flip seq) (return T)

instance Forceable ByteString
instance Forceable Char
instance Forceable Bool
instance Forceable Int
instance Forceable Word8

instance (Forceable a, Forceable b) => Forceable (a,b) where
    force (a,b) = force a >> force b

instance (Forceable a, Forceable b, Forceable c) => Forceable (a,b,c) where
    force (a,b,c) = force a >> force b >> force c

run c x tests = sequence_ $ zipWith (runTest c x) [1..] tests

runTest :: Int -> a -> Int -> (String,[F a]) -> IO ()
runTest count x n (name,tests) = do 
  printf "%2d " n
  fn tests
  printf "\t# %-16s\n" (show name)
  hFlush stdout
         where fn xs = case xs of
                         [f,g,h] -> runN count f x >> putStr "\t" 
                                  >> runN count g x >> putStr "\t"
                                  >> runN count h x >> putStr "\t"
                         [f,g]   -> runN count f x >> putStr "\t"
                                 >> runN count g x >> putStr "\t\t"
                         [f]     -> runN count f x >> putStr "\t\t\t"
                         _       -> return ()
               run f x = performGC >> threadDelay 100 >> time f x
               runN 0 f x = return ()
               runN c f x = run f x >> runN (c-1) f x

time (Flist f) a = do 
  start <- getCPUTime
  v     <- seqList (f a)
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / 10^12
  printf "%0.3f" (diff :: Double)
  hFlush stdout
                    
time (F f) a = do
  start <- getCPUTime
  v <- evaluate (f a)
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / 10^12
  printf "%0.3f" (diff :: Double)
  hFlush stdout

app1 f (x,y,z) = f x
app2 f (x,y,z) = f y
app3 f (x,y,z) = f z 
