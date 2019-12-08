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
            , part2 = Layer . foldr1 mergeLayers
            }

layers = chunksOf $ 25 * 6

count = (length .) . filter . (==)

fewestZeroes = minimumOn $ count 0

onesTimesTwos = product . (map count [1, 2] <*>) . pure

mergeLayers = (map mergePixels .) . zip

mergePixels (0, _) = 0
mergePixels (1, _) = 1
mergePixels (2, p) = p

data Layer =
    Layer [Int]

instance Show Layer where
    show (Layer l) =
        concat . map ('\n' :) . chunksOf 25 . map ([' ', '#'] !!) $ l
