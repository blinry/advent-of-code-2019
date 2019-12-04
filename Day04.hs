module Day04 where

import Common
import Data.List
import Data.List.Split

main =
    aoc 4
        Solution
            { parse = map show . range . map read . splitOn "-"
            , part1 = length . filter hasAdjacents . filter isSorted
            , part2 = length . filter hasAdjacents2 . filter isSorted
            }

range :: [Int] -> [Int]
range [a, b] = [a .. b]

isSorted xs = sort xs == xs

hasAdjacents xs = any ((> 1) . length) $ group xs

hasAdjacents2 xs = any ((== 2) . length) $ group xs
