import Control.Exception
import Control.Monad
import Data.Time.Clock
import Data.ByteString as B
import Data.Text.Encoding as T
import Data.Text as T
import System.Environment

time act = do
  start <- getCurrentTime
  act
  end <- getCurrentTime
  let d = diffUTCTime end start
  print d

main = do
  args <- getArgs
  forM_ args $ \f -> do
      t <- T.decodeUtf8 `fmap` B.readFile f
      evaluate t
      time $ evaluate (T.toUpper t)
