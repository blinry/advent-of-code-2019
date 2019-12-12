module Intcode where

import Common
import Data.Foldable
import Data.IntMap.Lazy (IntMap(..))
import qualified Data.IntMap.Lazy as IntMap
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Universe.Helpers

data Computer =
    Computer
        { mem :: IntMap Int
        , pos :: Int
        , offset :: Int
        , running :: Bool
        , input :: Seq Int
        , output :: Seq Int
        }
    deriving (Show)

fromList xs =
    Computer
        { mem = IntMap.fromList $ zip [0 ..] xs
        , pos = 0
        , offset = 0
        , running = True
        , input = Seq.empty
        , output = Seq.empty
        }

memory c i = IntMap.findWithDefault 0 i (mem c)

index m i = IntMap.findWithDefault 0 i m

getArgs :: Computer -> [Int]
getArgs Computer {pos = p, offset = o, mem = m} =
    map (\(arg, mode) -> [index m arg, arg, index m (arg + o), arg + o] !! mode) $
    argsWithModes
  where
    (op, modes) = opParse $ index m p
    size = opSize op
    allModes = modes ++ replicate (size - length modes) 0
    -- For some ops, never use "index" above
    fixedModes
        | op `elem` [1, 2, 3, 7, 8] && last allModes `elem` [0, 2] =
            init allModes ++ [(last allModes) + 1]
        | otherwise = allModes
    directArgs = map (index m) [p + 1 .. p + size]
    argsWithModes = zip directArgs fixedModes

step c@(Computer {pos = p, offset = o, mem = m, input = inp, output = out}) =
    Computer
        { pos = p'
        , offset = o'
        , mem = m'
        , running = r'
        , input = inp'
        , output = out'
        }
  where
    (op, modes) = opParse $ index m p
    args = getArgs c
    size = opSize op
    m' =
        case op of
            1 -> IntMap.insert c (a + b) m
                where [a, b, c] = args
            2 -> IntMap.insert c (a * b) m
                where [a, b, c] = args
            3 -> IntMap.insert a i m
                where [a] = args
                      i = Seq.index inp 0
            7 ->
                IntMap.insert
                    c
                    (case a < b of
                         True -> 1
                         False -> 0)
                    m
                where [a, b, c] = args
            8 ->
                IntMap.insert
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
    o' =
        case op of
            9 -> o + a
                where [a] = args
            _ -> o
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
        c {input = (input c) Seq.>< Seq.fromList (inp), output = Seq.empty}

opSize 99 = 0
opSize op = [3, 3, 1, 1, 2, 2, 3, 3, 1] !! (op - 1)

opParse :: Int -> (Int, [Int])
opParse code = (op, modes)
  where
    op = read $ reverse $ take 2 $ reverse $ s
    modes = reverse $ map (read . (: "")) $ take (length s - 2) $ s
    s = show code
