{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import Data.Monoid (mappend, mconcat)
import Prelude hiding (putStr)
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import Data.Text.Lazy.IO (putStr)

import qualified Data.Text as T

main :: IO ()
main = do
  putStr "Content-Type: text/html\n\n<table>"
  putStr . toLazyText $ mconcat (replicate 20000 makeRow) 
  putStr "</table>"

makeRow :: Builder
makeRow = mconcat (map makeCol [1..50])

makeCol :: Int -> Builder
makeCol 1 = fromText "<tr><td>1</td>"
makeCol 50 = fromText "<td>50</td></tr>"
makeCol i = fromText "<td>" `mappend` (textInt i `mappend` fromText "</td>")

textInt :: Int -> Builder
textInt = fromText . T.pack . show
