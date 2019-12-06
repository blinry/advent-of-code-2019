module Day06 where

import Common
import Data.List.Split
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

main = aoc 6 Solution {parse = parseOrbits, part1 = orbits, part2 = tbd}

parseOrbits =
    foldl (\m ([a, b]) -> M.alter (f b) a m) M.empty . map (splitOn ")") . lines
  where
    f b Nothing = Just [b]
    f b (Just xs) = Just $ b : xs

orbits m = orbits' m 0 "COM"

orbits' m p x = (p +) . sum . map (orbits' m (p + 1)) $ children
  where
    children = maybe [] id (M.lookup x m)
