-- Regression tests for specific bugs.

import Control.Exception (bracket)
import System.Directory (removeFile)
import System.IO (hClose, openTempFile)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE
import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F

-- Reported by Michael Snoyman: UTF-8 encoding a large lazy bytestring
-- caused either a segfault or attempt to allocate a negative number
-- of bytes.
lazy_encode_crash =
  bracket (openTempFile "." "crashy.txt")
          (\(path,h) -> hClose h >> removeFile path) $
  \(_,h) -> LB.hPut h . LE.encodeUtf8 . LT.pack . replicate 100000 $ 'a'

tests :: F.Test
tests = F.testGroup "crashers" [
          F.testCase "lazy_encode_crash" lazy_encode_crash
        ]

main = F.defaultMain [tests]
