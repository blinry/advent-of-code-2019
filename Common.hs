module Common where

import Linear.V2
import Text.Printf

data Solution a b c =
    Solution
        { parse :: String -> a
        , part1 :: a -> b
        , part2 :: a -> c
        }

aoc :: (Show b, Show c) => Int -> Solution a b c -> IO ()
aoc n solution = do
    input <- readFile $ (printf "%02d" n) ++ ".txt"
    let inputWithoutNewline = init input
    let problem = (parse solution) inputWithoutNewline
    putStrLn $ "Part 1: " ++ show (part1 solution $ problem)
    putStrLn $ "Part 2: " ++ show (part2 solution $ problem)

tbd x = "(not implemented)"

type Point = V2 Int
