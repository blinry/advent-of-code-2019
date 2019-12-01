module Day01 where

import Common

main =
    aoc 1
        Solution
            { parse = map read . lines
            , part1 = sum . map fuel
            , part2 = sum . map totalFuel
            }

fuel = max 0 . subtract 2 . (`div` 3)

totalFuel = sum . takeWhile (> 0) . tail . iterate fuel
