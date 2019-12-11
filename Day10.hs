module Day10 where

import Common
import Control.Lens.Indexed
import Data.Fixed
import Data.List.Extra
import qualified Data.Set as S
import Data.Set (Set)
import Linear.V2
import Linear.Vector

main =
    aoc 10
        Solution {parse = parseField, part1 = mostVisibleCoord, part2 = bet 200}

parseField = ifoldMap parseLine . lines

parseLine y = ifoldMap (parseChar y)

parseChar y x '#' = S.singleton (V2 x y)
parseChar _ _ _ = S.empty

between a b = map ((+ a) . (step ^*)) [1 .. (n - 1)]
  where
    d = b - a
    V2 dx dy = abs d
    n = gcd dx dy
    step = fmap (`div` n) d

visible m a b = S.null $ S.intersection (S.fromList $ between a b) m

countVisible m a = subtract 1 $ S.size $ S.filter (visible m a) m

mostVisible m = maximum $ S.map (countVisible m) m

mostVisibleCoord m =
    maximumBy (\a b -> compare (countVisible m a) (countVisible m b)) m

angle (V2 x y) = mod' (atan2 (fromIntegral x) (fromIntegral (-y))) (2 * pi)

directions n =
    sortOn
        Day10.angle
        [ V2 x y
        | x <- [(-m) .. m]
        , y <- [(-m) .. m]
        , not (x == 0 && y == 0)
        , gcd x y == 1
        ]
  where
    m = n - 1

hit m x d = headMay $ filter (flip S.member m) (between x (x + 40 * d))

headMay [] = Nothing
headMay (x:xs) = Just x

shoot m x d =
    case hit m x d of
        Nothing -> m
        Just y -> S.delete y m

completeVaporizationSteps m x =
    nub $ scanl (\m' d -> shoot m' x d) m (cycle (directions 40))

bet n m = encode lastVaporized
  where
    x = mostVisibleCoord m
    steps = take (n + 1) $ completeVaporizationSteps m x
    lastStep = last steps
    secondLastStep = last (init steps)
    lastVaporized = S.elemAt 0 (S.difference secondLastStep lastStep)
    encode (V2 x y) = x * 100 + y
