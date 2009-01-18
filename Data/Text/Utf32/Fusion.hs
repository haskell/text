{-# LANGUAGE BangPatterns #-}

module Data.Text.Utf32.Fusion where
    
import Data.Text.Fusion hiding (stream, unstream)
import Data.Text.Utf32.Internal
import Data.Text.UnsafeChar
import Data.Array.Base
import Data.Word
import Data.Array.ST
import Control.Monad.ST
import Char
import Control.Monad

stream :: Text -> Stream Char
stream (Text arr off len) = Stream next off len
    where    
      end = off+len
      {-# INLINE next #-}
      next !i
          | i >= end  = Done
          | otherwise = Yield (unsafeChr32 (arr `unsafeAt` i)) (i+1)
{-# INLINE [0] stream #-}

unstream :: Stream Char -> Text
unstream (Stream next0 s0 len) = x `seq` Text (fst x) 0 (snd x)
    where
      x :: ((UArray Int Word32),Int)
      x = runST ((unsafeNewArray_ (0,len) :: ST s (STUArray s Int Word32)) 
                 >>= (\arr -> loop arr 0 (len) s0))
      loop arr !i !max !s
          | i > max = do arr' <-unsafeNewArray_ (0,max*2)
                         copy arr arr'
                         loop arr' i (max*2) s
          | otherwise = case next0 s of
               Done       -> liftM2 (,) (unsafeFreezeSTUArray arr) (return i)
               Skip s'    -> loop arr i max s'
               Yield x s' -> do
                 unsafeWrite arr i n
                 loop arr (i+1) max s'
                   where
                     n :: Word32
                     n = fromIntegral $ ord x
{-# INLINE [0] unstream #-}

{-# RULES 
"STREAM stream/unstream fusion" forall s. 
   stream (unstream s) = s 
 #-}
