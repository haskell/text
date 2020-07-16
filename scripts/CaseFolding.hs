-- This script processes the following source file:
--
--   http://unicode.org/Public/UNIDATA/CaseFolding.txt

module CaseFolding
    (
      CaseFolding(..)
    , Fold(..)
    , parseCF
    , mapCF
    ) where

import Arsec

import Data.Char (ord)
import Data.Maybe (mapMaybe)
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Map as Map

data Fold = Fold {
      code :: Char
    , status :: Char
    , mapping :: [Char]
    , name :: String
    } deriving (Eq, Ord, Show)

data CaseFolding = CF { cfComments :: [Comment], cfFolding :: [Fold] }
                 deriving (Show)

entries :: Parser CaseFolding
entries = CF <$> many comment <*> many (entry <* many comment)
  where
    entry = Fold <$> unichar <* semi
                 <*> oneOf "CFST" <* semi
                 <*> unichars
                 <*> (string "# " *> manyTill anyToken (char '\n'))

parseCF :: FilePath -> IO (Either ParseError CaseFolding)
parseCF name = parse entries name <$> readFile name

-- We generate mapping trying to call toLower
mapCF :: [Map.Map Char (Char,Char,Char)] -> CaseFolding -> [String]
mapCF dbs (CF _ ms) = concat
    [ typ
    , mapMaybe nice [ minBound .. maxBound ]
    , [last]
    ]
  where
    -- characters for which base's toLower has different results
    different :: Set.Set Char
    different
        = Map.keysSet
        $ Map.filter g
        $ L.foldl' (alignWith f) (Map.map ToChar dbh) dbt
      where
        dbh : dbt = map (Map.mapMaybeWithKey h) dbs

        -- Only valid case is when both lhs and rhs of alignment
        -- have the same single character.
        -- Everything else is consider as different.
        --
        f :: These Maps Char -> Maps
        f (This _)              = Differently
        f (That _)              = Differently
        f (These Differently _) = Differently
        f (These (ToChar l) l')
            | l == l'           = ToChar l
            | otherwise         = Differently

        -- We are only interested in Differently
        -- for these we cannot trust toLower
        g :: Maps -> Bool
        g Differently = True
        g (ToChar _)  = False

        -- look for toLower data only
        h c (_, l, _) | c == l    = Nothing
                      | otherwise = Just l

    -- we are only interested in C and F cases
    -- * C: common case folding
    -- * F: full case folding
    --
    -- Case Folding says
    --
    -- Usage:
    --   A. To do a simple case folding, use the mappings with status C + S.
    --   B. To do a full case folding, use the mappings with status C + F.
    --
    folds :: Map.Map Char Fold
    folds = Map.fromList
          $ map (\f -> (code f, f))
          $ filter (\f -> status f `elem` "CF") ms

    -- there are three cases:
    --
    nice :: Char -> Maybe String
    nice c
        -- not mapping to toLower, and toLower is same for all GHCs
        | s /= [toLower c], not isDifferent
        = Just
        $ "-- " ++ n ++ "\n" ++
          "foldMapping " ++ showC c ++ " s = Yield " ++ x ++ " (CC s " ++ y ++ " " ++ z ++ ")"

        -- when toLower cannot be trusted
        | isDifferent
        = Just
        $ "-- " ++ n ++ "\n" ++
          "foldMapping " ++ showC c ++ " s = Yield " ++ x ++ " (CC s " ++ y ++ " " ++ z ++ ")"

        -- otherwise omit, to be handled by catch all toLower case.
        | otherwise
        = Nothing
      where
        s :: [Char] -- mapping
        n :: String -- name
        (n, s) = maybe (defName, [c]) (\f -> (name f, mapping f)) (Map.lookup c folds)

        isDifferent = Set.member c different

        [x,y,z] = (map showC . take 3) (s ++ repeat '\0')

        defName = "NOT FOLDED TO toLower " ++ showC c

    typ = ["foldMapping :: forall s. Char -> s -> Step (CC s) Char"
           ,"{-# NOINLINE foldMapping #-}"]
    last = "foldMapping c s = Yield (toLower c) (CC s '\\0' '\\0')"

-- auxiliary data type used to determine whether toLower is the same for a char
data Maps
    = ToChar Char
    | Differently
  deriving Show

alignWith :: Ord k => (These a b -> c) -> Map.Map k a -> Map.Map k b -> Map.Map k c
alignWith f = Map.mergeWithKey
    (\_ x y -> Just $ f $ These x y)
    (Map.map (f . This))
    (Map.map (f . That))

data These a b = This a | That b | These a b deriving Show
