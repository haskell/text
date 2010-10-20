import System.Environment
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

func :: (Eq a) => [a] -> IO ()
func ls =
  print . sum . map (\needle -> length . filter (==needle) $ ls) $ take 100 ls

bytestring haystack = func =<< B.lines `fmap` B.readFile haystack

lazyBytestring haystack = func =<< BL.lines `fmap` BL.readFile haystack

text haystack = func =<< (T.lines . T.decodeUtf8) `fmap` B.readFile haystack

lazyText haystack = func =<<
                    (TL.lines . TL.decodeUtf8) `fmap` BL.readFile haystack

string haystack = func =<< lines `fmap` readFile haystack

main = do
  args <- getArgs
  case args of
    ["bs",h] -> bytestring h
    ["lazybs",h] -> lazyBytestring h
    ["text",h] -> text h
    ["lazytext",h] -> lazyText h
    ["string",h] -> string h
