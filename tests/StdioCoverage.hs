import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main = do
  args <- getArgs
  case args of
    ["T.readFile", name] -> T.putStr =<< T.readFile name
    ["T.writeFile", name, t] -> T.writeFile name (T.pack t)
    ["T.appendFile", name, t] -> T.appendFile name (T.pack t)
    ["T.interact"] -> T.interact id
    ["T.getContents"] -> T.putStr =<< T.getContents
    ["T.getLine"] -> T.putStrLn =<< T.getLine

    ["TL.readFile", name] -> TL.putStr =<< TL.readFile name
    ["TL.writeFile", name, t] -> TL.writeFile name (TL.pack t)
    ["TL.appendFile", name, t] -> TL.appendFile name (TL.pack t)
    ["TL.interact"] -> TL.interact id
    ["TL.getContents"] -> TL.putStr =<< TL.getContents
    ["TL.getLine"] -> TL.putStrLn =<< TL.getLine
    _ -> hPutStrLn stderr "invalid directive!" >> exitFailure
