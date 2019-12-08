module Day08 where

import Common
import Data.List
import Data.List.Extra
import Data.List.Singleton

main =
    aoc 8
        Solution
            { parse = layers . map (read . singleton)
            , part1 = onesTimesTwos . fewestZeroes
            , part2 = Layer . mergeLayers
            }

layers = chunksOf $ 25 * 6

count = (length .) . filter . (==)

fewestZeroes = minimumOn $ count 0

onesTimesTwos = product . (map count [1, 2] <*>) . pure

mergeLayers = map (head . filter (/= 2)) . transpose

data Layer =
    Layer [Int]

instance Show Layer where
    show (Layer l) =
        concat . map ('\n' :) . chunksOf 25 . map ([' ', '#'] !!) $ l
