{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Text.Lazy
    (
      Text
    , pack
    , unpack
    -- * Basic interface
    , append
    ) where

import Data.String (IsString(..))
import qualified Data.Text.Fusion as S
import Data.Text.Fusion.Internal as S
import Data.Text.Lazy.Fusion
import Data.Text.Lazy.Internal

instance Eq Text where
    t1 == t2 = stream t1 `S.eq` stream t2

instance Show Text where
    showsPrec p ps r = showsPrec p (unpack ps) r

instance Read Text where
    readsPrec p str = [(pack x,y) | (x,y) <- readsPrec p str]

instance IsString Text where
    fromString = pack

-- | /O(n)/ Convert a 'String' into a 'Text'.
--
-- This function is subject to array fusion.
pack :: String -> Text
pack = unstream . S.streamList
{-# INLINE [1] pack #-}

-- | /O(n)/ Convert a 'Text' into a 'String'.
-- Subject to array fusion.
unpack :: Text -> String
unpack = unstreamList . stream
{-# INLINE [1] unpack #-}

-- | /O(n\/c)/ Append two 'Text's
append :: Text -> Text -> Text
append xs ys = foldrChunks Chunk ys xs
{-# INLINE append #-}

{-# RULES
"TEXT append -> fused" [~1] forall t1 t2.
    append t1 t2 = unstream (S.append (stream t1) (stream t2))
"TEXT append -> unfused" [1] forall t1 t2.
    unstream (S.append (stream t1) (stream t2)) = append t1 t2
 #-}
