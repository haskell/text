{-# OPTIONS_GHC -fbang-patterns -fglasgow-exts #-}

module Text.Utf8.Fusion where

import Data.Array.Base
import Data.Word
import Control.Monad.ST
import Data.Text.UnsafeChar
import Control.Monad
import Char

import Text.Utf8
import Text.Utf8.Internal
import Text.Fusion hiding (stream,unstream)

stream :: Text -> Stream Char
stream (Text arr off len) = Stream next off len
    where
      end = off+len
      {-# INLINE next #-}
      next !i
          | i >= end = Done
          | n <= 0x7F = Yield (unsafeChr8 n)    (i + 1)
          | n <= 0xDF = Yield (chr2 n n2)       (i + 2)
          | n <= 0xEF = Yield (chr3 n n2 n3)    (i + 3)
          | otherwise = Yield (chr4 n n2 n3 n4) (i + 4)
          where
            n  = arr `unsafeAt`  i
            n2 = arr `unsafeAt` (i + 1)
            n3 = arr `unsafeAt` (i + 2)
            n4 = arr `unsafeAt` (i + 3)
{-# INLINE [0] stream #-}
      

unstream :: Stream Char -> Text
unstream (Stream next0 s0 len) = x `seq` (Text (fst x) 0 (snd x))
    where
      x :: ((UArray Int Word8), Int)
      x = runST ((unsafeNewArray_ (0,len+4) :: ST s (STUArray s Int Word8))
                  >>= (\arr -> loop arr 0 (len+4) s0))
      loop !arr !i !max !s
          | i + 4 > max = do arr' <- unsafeNewArray_ (0,max*2)
                             copy arr arr'
                             loop arr' i (max*2) s
          | otherwise = case next0 s of
               Done       -> liftM2 (,) (unsafeFreezeSTUArray arr) (return i)
               Skip s'    -> loop arr i max s'
               Yield x s' 
                   | n <= 0x7F -> do
                        unsafeWrite arr i n
                        loop arr (i+1) max s'
                   | n <= 0x07FF -> do
                        unsafeWrite arr i     (fst n2)
                        unsafeWrite arr (i+1) (snd n2)
                        loop arr (i+2) max s'
                   | n <= 0xFFFF -> do
                        unsafeWrite arr i     (fst3 n3)
                        unsafeWrite arr (i+1) (snd3 n3)
                        unsafeWrite arr (i+2) (trd3 n3)
                        loop arr (i+3) max s'
                   | otherwise  -> do
                       unsafeWrite arr i     (fst4 n4)
                       unsafeWrite arr (i+1) (snd4 n4)
                       unsafeWrite arr (i+2) (trd4 n4)
                       unsafeWrite arr (i+3) (fth4 n4)
                       loop arr (i+4) max s'
                   where
                     n  = (fromIntegral . ord) x :: Word8
                     n2 = ord2 x
                     n3 = ord3 x
                     n4 = ord4 x  
                     fst3 !x = let (x1,_,_)   = x in x1
                     snd3 !x = let (_,x2,_)   = x in x2
                     trd3 !x = let (_,_,x3)   = x in x3
                     fst4 !x = let (x1,_,_,_) = x in x1   
                     snd4 !x = let (_,x2,_,_) = x in x2
                     trd4 !x = let (_,_,x3,_) = x in x3
                     fth4 !x = let (_,_,_,x4) = x in x4
{-# INLINE [0] unstream #-}
                    
{-# RULES 
"STREAM stream/unstream fusion" forall s. 
   stream (unstream s) = s 
 #-}