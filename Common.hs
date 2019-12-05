{-# LANGUAGE OverloadedStrings #-}

module Common where

import Control.Exception
import qualified Data.Map as Map
import Data.Map (Map)
import Formatting
import Formatting.Clock
import Linear.V2
import System.Clock
import Text.Printf

data Solution a b c =
    Solution
        { parse :: String -> a
        , part1 :: a -> b
        , part2 :: a -> c
        }

benchmark :: IO a -> IO ()
benchmark action = do
    start <- getTime Monotonic
    action
    end <- getTime Monotonic
    fprint (" (" % timeSpecs % ")\n") start end

aoc :: (Show a, Show b, Show c) => Int -> Solution a b c -> IO ()
aoc n solution = do
    input <- readFile $ (printf "%02d" n) ++ ".txt"
    let inputWithoutNewline = init input
    let problem = parse solution $ inputWithoutNewline
    benchmark $ do
        putStr "Parsing input..."
        evaluate problem
    benchmark $ putStr $ "Part 1: " ++ show (part1 solution $ problem)
    benchmark $ putStr $ "Part 2: " ++ show (part2 solution $ problem)

tbd x = "(not implemented)"

freqs :: Ord a => [a] -> Map a Integer
freqs = Map.fromListWith (+) . map ((flip (,)) 1)
