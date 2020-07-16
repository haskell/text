-- This script is used to dump casing DB from GHCs base library

import Data.Char

main :: IO ()
main = print
    [ (c, u, l, t)
    | c <- [ minBound .. maxBound ]
    , let u = toUpper c
    , let l = toLower c
    , let t = toTitle c

    -- we dump only characters which have some transformations
    , c /= u || c /= l || c /= t
    ]
