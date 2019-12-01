module Day01 where

import Common

main =
    aoc 1
        Solution {parse = map read . lines, part1 = sum . map fuel, part2 = tbd}

fuel mass = mass `div` 3 - 2
