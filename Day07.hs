module Day07 where

import Common
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Universe.Helpers
import Day05 hiding (main)

main =
    aoc 7
        Solution
            { parse = Day05.fromList . map read . splitOn ","
            , part1 = findMaxOutput
            , part2 = findMaxOutput2
            }

tryPhaseSetting c phases =
    foldl amplify 0 $ zip phases $ replicate (length phases) c
  where
    amplify input (phase, computer) = head $ run [phase, input] computer

findMaxOutput c = maximum $ map (tryPhaseSetting c) $ permutations [0 .. 4]

phaseInit :: [Computer] -> [Int] -> [Computer]
phaseInit cs phases =
    map (\(c, p) -> c {input = Seq.singleton p}) $ zip cs phases

phaseLoop :: ([Computer], Int, Bool) -> ([Computer], Int, Bool)
phaseLoop (cs, init, _) =
    case running of
        False -> (cs', init, running)
        True -> (cs', fromJust out, running)
  where
    (cs', out, running) = foldl amplify ([], Just init, True) cs
    amplify (cs', input, running) c =
        case input of
            Nothing -> (cs', Nothing, False)
            Just i ->
                case out of
                    Nothing -> (cs', Nothing, False)
                    Just i -> (cs' ++ [c'], Just i, True)
                where (c', out) = nextOutput i c

tryPhaseSetting2 c phases =
    (\(_, out, _) -> out) $
    until (\(_, _, running) -> running == False) phaseLoop (c', 0, True)
  where
    c' = phaseInit (replicate (length phases) c) phases

findMaxOutput2 c = maximum $ map (tryPhaseSetting2 c) $ permutations [5 .. 9]
