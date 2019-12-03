module Day03 where

import Common
import Data.List.Split
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Linear

main =
    aoc 3
        Solution
            { parse = map (scanl1 (+) . parseWire) . lines
            , part1 = minimum . Set.map manhattan . crossings . map points
            , part2 = minimum . crossings2 . map pointsWithTimes
            }

parseDirection (x:xs) =
    replicate (read xs) $
    case x of
        'R' -> V2 1 0
        'L' -> V2 (-1) 0
        'D' -> V2 0 1
        'U' -> V2 0 (-1)

parseWire = concatMap parseDirection . splitOn ","

points s = Set.fromList s

pointsWithTimes s = Map.fromListWith min $ zip s [1 ..]

crossings = foldr1 Set.intersection

crossings2 = foldr1 (Map.intersectionWith (+))

manhattan (V2 x y) = abs x + abs y
