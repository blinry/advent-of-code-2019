module Day02 where

import Common
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Universe.Helpers

main =
    aoc 2
        Solution
            { parse = map read . splitOn ","
            , part1 = head . run . modify (12, 02)
            , part2 = checksum . findInput 19690720
            }

checksum x = 100 * fst x + snd x

findInput output program = fromJust $ find (hasOutput program output) allPairs

hasOutput program output input = output == (head . run $ modify input program)

allPairs = [0 ..] +*+ [0 ..]

modify (noun, verb) (a:_:_:xs) = a : noun : verb : xs

get (xs, ys) i = (xs ++ ys) !! i

put' xs i v = xs' ++ v : ys'
  where
    (xs', y:ys') = splitAt i xs

put (xs, ys) i v
    | i < length xs = (put' xs i v, ys)
    | otherwise = (xs, put' ys (i - length xs) v)

run xs = fst $ until (null . snd) step ([], xs)

step (xs, []) = (xs, [])
step (xs, 99:ys) = (xs ++ 99 : ys, [])
step state@(xs, ys@(op:a:b:c:_)) =
    shift $ put state c $ operator $ map (get state) [a, b]
  where
    operator =
        case op of
            1 -> sum
            2 -> product

shift (xs, a:b:c:d:ys) = (xs ++ [a, b, c, d], ys)
