import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

text needle haystack = do
  bs <- B.readFile haystack
  print . length . filter (==needle) . T.lines . T.decodeUtf8 $ bs

lazyText needle haystack = do
  bs <- BL.readFile haystack
  print . length . filter (==needle) . TL.lines . TL.decodeUtf8 $ bs

string needle haystack = do
  s <- readFile haystack
  print . length . filter (==needle) . lines $ s

main = do
  args <- getArgs
  case args of
    ["text",n,h] -> text (T.pack n) h
    ["lazytext",n,h] -> lazyText (TL.pack n) h
    ["string",n,h] -> string n h
