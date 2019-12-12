module Day11 where

import Common
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Intcode
import Linear.V2

main =
    aoc 11
        Solution
            { parse = Intcode.fromList . map read . splitOn ","
            , part1 = M.size . hull . paint . initialState
            , part2 = Hull . hull . paint . initialCorrectState
            }

type Point = V2 Int

data Hull =
    Hull (Map Point Int)

data State =
    State
        { hull :: Map Point Int
        , position :: Point
        , direction :: Point
        , brain :: Computer
        }

initialState c =
    State {hull = M.empty, position = V2 0 0, direction = V2 0 (-1), brain = c}

initialCorrectState c = (initialState c) {hull = M.fromList [(V2 0 0, 1)]}

nextState State {hull = h, position = p, direction = d, brain = b} =
    State {hull = h', position = p', direction = d', brain = b''}
  where
    color = M.findWithDefault 0 p h
    (b', maybeColor') = nextOutput [color] b
    (b'', maybeTurn) = nextOutput [] b'
    h' =
        case maybeColor' of
            Just color' -> M.insert p color' h
            _ -> h
    d' =
        case maybeTurn of
            Just turn -> doTurn d turn
            _ -> d
    p' =
        case maybeTurn of
            Just turn -> p + d'
            _ -> p

paint s = until (not . running . brain) nextState s

doTurn (V2 0 1) 0 = V2 1 0
doTurn (V2 1 0) 0 = V2 0 (-1)
doTurn (V2 0 (-1)) 0 = V2 (-1) 0
doTurn (V2 (-1) 0) 0 = V2 0 1
doTurn (V2 1 0) 1 = V2 0 1
doTurn (V2 0 (-1)) 1 = V2 1 0
doTurn (V2 (-1) 0) 1 = V2 0 (-1)
doTurn (V2 0 1) 1 = V2 (-1) 0

instance Show Hull where
    show (Hull m) =
        ('\n' :) $ intercalate "\n" $ map (\y -> showLine y m) [yMin .. yMax]
      where
        showLine y m =
            map toChar $
            map (\x -> M.findWithDefault 0 (V2 x y) m) [xMin .. xMax]
        toChar 0 = ' '
        toChar 1 = '#'
        (V2 xMin _) =
            minimumBy (\(V2 x1 y1) (V2 x2 y2) -> compare x1 x2) $ M.keys m
        (V2 xMax _) =
            maximumBy (\(V2 x1 y1) (V2 x2 y2) -> compare x1 x2) $ M.keys m
        (V2 _ yMin) =
            minimumBy (\(V2 x1 y1) (V2 x2 y2) -> compare y1 y2) $ M.keys m
        (V2 _ yMax) =
            maximumBy (\(V2 x1 y1) (V2 x2 y2) -> compare y1 y2) $ M.keys m
