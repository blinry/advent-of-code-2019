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

run xs = run' ([], xs)
  where
    run' (xs, []) = xs
    run' (xs, ys) = run' $ shift $ op (xs, ys)

op (xs, 99:ys) = (xs ++ 99 : ys, [])
op (xs, ys@(1:a:b:c:_)) = put (xs, ys) c v
  where
    v = get (xs, ys) a + get (xs, ys) b
op (xs, ys@(2:a:b:c:_)) = put (xs, ys) c v
  where
    v = get (xs, ys) a * get (xs, ys) b

shift (xs, a:b:c:d:ys) = (xs ++ [a, b, c, d], ys)
shift (xs, []) = (xs, [])
