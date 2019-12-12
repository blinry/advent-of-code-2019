module Day12 where

import Common
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Void
import Linear.V3
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Point = V3 Int

type Parser = Parsec Void String

data Moon =
    Moon
        { vel :: Point
        , pos :: Point
        }
    deriving (Show, Eq, Ord)

main =
    aoc 12
        Solution
            { Common.parse = map parseMoon . lines
            , part1 = energy . flip (!!) 1000 . iterate step
            , part2 =
                  \moons ->
                      foldr1
                          lcm
                          [ findRepeatX moons
                          , findRepeatY moons
                          , findRepeatZ moons
                          ]
            }

step moons = map (\m -> move m (delete m moons)) moons

findRepeat :: [Moon] -> Int
findRepeat moons = i
  where
    seenAlready (moons, known, _) = S.member moons known
    (_, _, i) = until seenAlready stepWithMap (moons, S.empty, 0)

findRepeatX :: [Moon] -> Int
findRepeatX moons = i
  where
    seenAlready (moons, known, _) = S.member moons known
    (_, _, i) = until seenAlready stepxWithMap (moons, S.empty, 0)

findRepeatY :: [Moon] -> Int
findRepeatY moons = i
  where
    seenAlready (moons, known, _) = S.member moons known
    (_, _, i) = until seenAlready stepyWithMap (moons, S.empty, 0)

findRepeatZ :: [Moon] -> Int
findRepeatZ moons = i
  where
    seenAlready (moons, known, _) = S.member moons known
    (_, _, i) = until seenAlready stepzWithMap (moons, S.empty, 0)

stepWithMap (moons, known, i) = (moons', known', i + 1)
  where
    known' = S.insert moons known
    moons' = map (\m -> move m (delete m moons)) moons

stepxWithMap (moons, known, i) = (moons', known', i + 1)
  where
    known' = S.insert moons known
    moons' = map (\m -> movex m (delete m moons)) moons

stepyWithMap (moons, known, i) = (moons', known', i + 1)
  where
    known' = S.insert moons known
    moons' = map (\m -> movey m (delete m moons)) moons

stepzWithMap (moons, known, i) = (moons', known', i + 1)
  where
    known' = S.insert moons known
    moons' = map (\m -> movez m (delete m moons)) moons

move m@Moon {pos = V3 x y z, vel = V3 vx vy vz} others =
    m {pos = V3 (x + vx') (y + vy') (z + vz'), vel = V3 vx' vy' vz'}
  where
    [xx, yy, zz] = coords others
    vx' = vx + effect x xx
    vy' = vy + effect y yy
    vz' = vz + effect z zz
    coords = transpose . map (\Moon {pos = V3 x y z} -> [x, y, z])

movex m@Moon {pos = V3 x y z, vel = V3 vx vy vz} others =
    m {pos = V3 (x + vx') y z, vel = V3 vx' vy vz}
  where
    [xx, yy, zz] = coords others
    vx' = vx + effect x xx
    coords = transpose . map (\Moon {pos = V3 x y z} -> [x, y, z])

movey m@Moon {pos = V3 x y z, vel = V3 vx vy vz} others =
    m {pos = V3 x (y + vy') z, vel = V3 vx vy' vz}
  where
    [xx, yy, zz] = coords others
    vy' = vy + effect y yy
    coords = transpose . map (\Moon {pos = V3 x y z} -> [x, y, z])

movez m@Moon {pos = V3 x y z, vel = V3 vx vy vz} others =
    m {pos = V3 x y (z + vz'), vel = V3 vx vy vz'}
  where
    [xx, yy, zz] = coords others
    vz' = vz + effect z zz
    coords = transpose . map (\Moon {pos = V3 x y z} -> [x, y, z])

effect x xx = (length $ filter (> x) xx) - (length $ filter (< x) xx)

energy = sum . map (\m -> kineticEnergy m * potentialEnergy m)
  where
    kineticEnergy Moon {vel = v} = sum $ abs v
    potentialEnergy Moon {pos = p} = sum $ abs p

signed :: Parser Int
signed = read <$> some (oneOf "-0123456789")

moon :: Parser Moon
moon = do
    string "<x="
    x <- signed
    string ", y="
    y <- signed
    string ", z="
    z <- signed
    string ">"
    pure Moon {pos = V3 x y z, vel = V3 0 0 0}

parseMoon = fromJust . parseMaybe moon
