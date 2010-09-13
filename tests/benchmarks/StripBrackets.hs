-- From Petr Prokhorenkov.

import Data.Text as T
import Data.Text.IO as T

stripBrackets :: T.Text -> T.Text
stripBrackets text = snd $ T.mapAccumL f 0 text where
   f depth c = let
       depth' = depth + d' c
       c' | depth > 0 || depth' > 0 = ' '
          | otherwise = c
       in
       (depth', c')

   d' '{' = 1
   d' '[' = 1
   d' '}' = -1
   d' ']' = -1
   d' _   = 0

main = T.interact stripBrackets
