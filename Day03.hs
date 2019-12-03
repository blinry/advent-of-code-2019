module Day03 where

import Common
import Data.Ix
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Linear

main =
    aoc 3
        Solution
            { parse = map parseWire . lines
            , part1 =
                  minimum .
                  Set.map manhattan . crossings . tupelize . map points
            , part2 = minimum . crossings2 . tupelize . map points2
            }

tupelize [a, b] = (a, b)

parseDirection (x:xs) =
    case x of
        'R' -> V2 l 0
        'L' -> V2 (-l) 0
        'D' -> V2 0 l
        'U' -> V2 0 (-l)
  where
    l = read xs

parseWire = map parseDirection . splitOn ","

range' (a, b)
    | a <= b = range (a, b)
    | otherwise = reverse $ range (b, a)

points xs = Set.fromList $ points' xs

points2 xs = Map.fromListWith min $ zip (points' xs) [1 ..]

points' [x] = [x]
points' xs = prev ++ delete pos (range' (pos, pos + d))
  where
    prev = points' (init xs)
    pos = last prev
    d = last xs

crossings (xs, ys) = Set.intersection xs ys

crossings2 (xs, ys) = Map.intersectionWith (+) xs ys

manhattan (V2 x y) = abs x + abs y
