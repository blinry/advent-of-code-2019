module Day01 where

import Common

main =
    aoc 1
        Solution
            { parse = map read . lines
            , part1 = sum . map fuel
            , part2 = sum . map totalFuel
            }

fuel mass = maximum [0, mass `div` 3 - 2]

totalFuel 0 = 0
totalFuel mass = fuel mass + totalFuel (fuel mass)
