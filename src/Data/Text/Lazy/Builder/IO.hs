module Data.Text.Lazy.Builder.IO
  ( hPutBuilder
  ) where

import System.IO (Handle)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import qualified Data.Text.Lazy.IO as L

-- | Write a Builder to a handle.
-- Currently, just a placeholder that uses LazyText until an implementation is added.
hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder h b = L.hPutStr h $ toLazyText b

