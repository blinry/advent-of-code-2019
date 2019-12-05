module Day05 where

import Common
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Universe.Helpers

data Computer =
    Computer
        { mem :: Seq Int
        , pos :: Int
        , running :: Bool
        , input :: Seq Int
        , output :: Seq Int
        }

fromList xs =
    Computer
        { mem = Seq.fromList xs
        , pos = 0
        , running = True
        , input = Seq.empty
        , output = Seq.empty
        }

memory c i = Seq.index (mem c) i

step c@(Computer {pos = p, mem = m, input = inp, output = out}) =
    c {pos = p', mem = m', running = r', input = inp', output = out'}
  where
    op = Seq.index m p
    size = opSize op
    args = map (Seq.index m . (+ p)) [1 .. size]
    m' =
        case op of
            1 -> Seq.update c (a + b) m
                where [a, b, c] = args
            2 -> Seq.update c (a * b) m
                where [a, b, c] = args
            3 -> Seq.update a i m
                where [a] = args
                      i = Seq.index inp 0
            4 -> m
            99 -> m
    r' = op /= 99
    p' = p + size + 1
    inp' =
        case op of
            1 -> Seq.deleteAt 0 inp
            _ -> inp
    out' =
        case op of
            4 -> out Seq.|> a
                where a = 42
            _ -> out

run c inp = toList $ output c''
  where
    c' = c {input = Seq.fromList inp}
    c'' = until (not . running) step c'

opSize op =
    case op of
        1 -> 3
        2 -> 3
        3 -> 1
        4 -> 1
        99 -> 0

main =
    aoc 5
        Solution
            { parse = Day05.fromList . map read . splitOn ","
            , part1 = tbd
            , part2 = tbd
            }
