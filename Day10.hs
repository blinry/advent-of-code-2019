module Day10 where

import Common
import Data.List.Extra
import qualified Data.Set as S
import Data.Set (Set)
import Linear.V2

main = aoc 10 Solution {parse = parseField, part1 = mostVisible, part2 = tbd}

parseField = S.unions . map (\(y, l) -> parseLine y l) . zip [0 ..] . lines

parseLine y s = S.fromList [V2 x y | (c, x) <- zip s [0 ..], c == '#']

between a b = S.fromList $ map (\i -> a + fmap (* i) step) [1 .. (n - 1)]
  where
    d = (b - a)
    V2 dx dy = abs d
    n = gcd dx dy
    step = fmap (`div` n) d

visible m a b = S.null $ S.intersection (between a b) m

countVisible m a = subtract 1 $ S.size $ S.filter (visible m a) m

mostVisible m = maximum $ S.map (countVisible m) m
