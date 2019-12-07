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
    deriving (Show)

fromList xs =
    Computer
        { mem = Seq.fromList xs
        , pos = 0
        , running = True
        , input = Seq.empty
        , output = Seq.empty
        }

memory c i = Seq.index (mem c) i

getArgs :: Computer -> [Int]
getArgs Computer {pos = p, mem = m} =
    map (\(arg, mode) -> [Seq.index m arg, arg] !! mode) $ argsWithModes
  where
    (op, modes) = opParse $ Seq.index m p
    size = opSize op
    allModes = modes ++ replicate (size - length modes) 0
    -- For some ops, always put the last operand in address mode.
    fixedModes
        | op `elem` [1, 2, 3, 7, 8] = init allModes ++ [1]
        | otherwise = allModes
    directArgs = map (Seq.index m) [p + 1 .. p + size]
    argsWithModes = zip directArgs fixedModes

step c@(Computer {pos = p, mem = m, input = inp, output = out}) =
    Computer {pos = p', mem = m', running = r', input = inp', output = out'}
  where
    (op, modes) = opParse $ Seq.index m p
    args = getArgs c
    size = opSize op
    m' =
        case op of
            1 -> Seq.update c (a + b) m
                where [a, b, c] = args
            2 -> Seq.update c (a * b) m
                where [a, b, c] = args
            3 -> Seq.update a i m
                where [a] = args
                      i = Seq.index inp 0
            7 ->
                Seq.update
                    c
                    (case a < b of
                         True -> 1
                         False -> 0)
                    m
                where [a, b, c] = args
            8 ->
                Seq.update
                    c
                    (case a == b of
                         True -> 1
                         False -> 0)
                    m
                where [a, b, c] = args
            _ -> m
    r' = op /= 99
    p' =
        case op of
            5 ->
                case a of
                    0 -> fallback
                    _ -> b
                where [a, b] = args
            6 ->
                case a of
                    0 -> b
                    _ -> fallback
                where [a, b] = args
            _ -> fallback
      where
        fallback = p + size + 1
    inp' =
        case op of
            3 -> Seq.deleteAt 0 inp
            _ -> inp
    out' =
        case op of
            4 -> out Seq.|> a
                where a = head args
            _ -> out

run inp c =
    toList $ output $ until (not . running) step c {input = Seq.fromList inp}

nextOutput inp c =
    (\c -> (c, Seq.lookup 0 (output c))) $
    until
        (\c -> (not . Seq.null . output $ c) || (not . running $ c))
        step
        c {input = (input c) Seq.|> inp, output = Seq.empty}

opSize 99 = 0
opSize op = [3, 3, 1, 1, 2, 2, 3, 3] !! (op - 1)

opParse :: Int -> (Int, [Int])
opParse code = (op, modes)
  where
    op = read $ reverse $ take 2 $ reverse $ s
    modes = reverse $ map (read . (: "")) $ take (length s - 2) $ s
    s = show code

main =
    aoc 5
        Solution
            { parse = Day05.fromList . map read . splitOn ","
            , part1 = run [1]
            , part2 = run [5]
            }
