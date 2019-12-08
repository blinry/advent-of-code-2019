module Day08 where

import Common
import Data.List
import Data.List.Extra

main =
    aoc 8
        Solution
            { parse = map (read . (: []))
            , part1 = onesTimesTwos . fewestZeroes . layers
            , part2 = tbd
            }

layers = chunksOf (25 * 6)

fewestZeroes = minimumOn (length . filter (== 0))

onesTimesTwos l = product $ map (\n -> length $ filter (== n) l) [1, 2]
