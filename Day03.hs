module Day03 where

import Common
import Data.Ix
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Linear

main =
    aoc 3
        Solution
            { parse = map parseWire . lines
            , part1 = manhattan . closest . crossings . map points
            , part2 = head . sort . map snd . crossingsTimes . map points
            }

parseDirection (x:xs) =
    case x of
        'R' -> V2 l 0
        'L' -> V2 (-l) 0
        'D' -> V2 0 l
        'U' -> V2 0 (-l)
  where
    l = read xs

parseWire = map parseDirection . splitOn ","

range' (a, b)
    | a <= b = range (a, b)
    | otherwise = reverse $ range (b, a)

points [] = [V2 0 0]
points xs = prev ++ delete pos (range' (pos, pos + d))
  where
    prev = points (init xs)
    pos = last prev
    d = last xs

intersect' xs ys =
    freqs $ filter (\p -> Map.lookup p ys /= Nothing) (Map.keys xs)

crossings = delete (V2 0 0) . Map.keys . foldr1 intersect' . map freqs

crossingsTimes xs =
    sumTimes timeses $ delete (V2 0 0) $ Map.keys (foldr1 intersect' $ timeses)
  where
    timeses = map Map.fromList (map times xs)
    sumTimes ts = map (\p -> (p, sum $ map (fromJust . Map.lookup p) ts))

manhattan (V2 x y) = abs x + abs y

closest xs = head $ sortOn manhattan xs

times xs = zip xs [0 ..]
