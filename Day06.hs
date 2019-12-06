module Day06 where

import Common
import Data.List.Split
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

main =
    aoc 6
        Solution
            { parse = parseOrbits
            , part1 = orbits
            , part2 = minTransfers "YOU" "SAN" . reverseOrbits
            }

parseOrbits =
    foldl (\m ([a, b]) -> M.alter (Just . (b :) . maybe [] id) a m) M.empty .
    map (splitOn ")") . lines

orbits m = orbits' m 0 "COM"

orbits' m p x = (p +) . sum . map (orbits' m (p + 1)) $ children
  where
    children = maybe [] id (M.lookup x m)

reverseOrbits m = M.foldlWithKey f M.empty m
  where
    f m' parent children = foldl (\m'' c -> M.insert c parent m'') m' children

path m x = reverse $ path' m x

path' m x =
    case parent of
        Nothing -> []
        Just x -> x : (path' m x)
  where
    parent = M.lookup x m

commonPrefix p1 p2 = map fst . takeWhile (\(a, b) -> a == b) $ zip p1 p2

minTransfers a b m = (length p1 - length common) + (length p2 - length common)
  where
    p1 = path m a
    p2 = path m b
    common = commonPrefix p1 p2
