module Day06 where

import Common
import Data.List.Split
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

main =
    aoc 6
        Solution
            { parse = id -- This solution uses different parsing per part, so we don't do that here.
            , part1 = orbits . parseOrbits
            , part2 = minTransfers "YOU" "SAN" . parseReverseOrbits
            }

parseOrbits =
    foldr (\[a, b] m -> M.alter (Just . (b :) . maybe [] id) a m) M.empty .
    map (splitOn ")") . lines

parseReverseOrbits =
    foldr (\[a, b] m -> M.insert b a m) M.empty . map (splitOn ")") . lines

orbits m = orbits' m 0 "COM"
  where
    orbits' m p x =
        (p +) . sum . map (orbits' m (p + 1)) $ M.findWithDefault [] x m

path m x = reverse $ path' m x
  where
    path' m x =
        case M.lookup x m of
            Nothing -> []
            Just x -> x : (path' m x)

commonPrefix p1 p2 = map fst . takeWhile (\(a, b) -> a == b) $ zip p1 p2

minTransfers a b m = (length p1 - length common) + (length p2 - length common)
  where
    p1 = path m a
    p2 = path m b
    common = commonPrefix p1 p2
