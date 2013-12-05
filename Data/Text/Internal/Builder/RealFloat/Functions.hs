-- |
-- Module:    Data.Text.Internal.Builder.RealFloat.Functions
-- Copyright: (c) The University of Glasgow 1994-2002
-- License:   see libraries/base/LICENSE
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!

module Data.Text.Internal.Builder.RealFloat.Functions
    (
      roundTo
    ) where

roundTo :: Int -> [Int] -> (Int,[Int])
roundTo d is =
  case f d is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> error "roundTo: bad Value"
 where
  f n []     = (0, replicate n 0)
  f 0 (x:_)  = (if x >= 5 then 1 else 0, [])
  f n (i:xs)
     | i' == 10  = (1,0:ds)
     | otherwise = (0,i':ds)
      where
       (c,ds) = f (n-1) xs
       i'     = c + i
