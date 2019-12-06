module Day00Spec where

import Common
import qualified Data.Map as M
import Data.Map (Map)
import Day06 hiding (main)
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "part1" $ do
                parseOrbits "a)b" `shouldBe` M.fromList [("a", ["b"])]
                parseOrbits "a)b\nb)c\na)c" `shouldBe`
                    M.fromList [("a", ["b", "c"]), ("b", ["c"])]
                orbits (parseOrbits "COM)a") `shouldBe` 1
                orbits (parseOrbits "COM)a\nCOM)b") `shouldBe` 2
                orbits (parseOrbits "COM)a\nCOM)b\nb)c\nb)d") `shouldBe` 6
                orbits
                    (parseOrbits
                         "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") `shouldBe`
                    42
            it "path" $ do
                let m = parseReverseOrbits "COM)a\nCOM)b\nb)x\nx)c\nb)d"
                let path1 = path m "d"
                let path2 = path m "c"
                path1 `shouldBe` ["COM", "b"]
                path2 `shouldBe` ["COM", "b", "x"]
                commonPrefix path1 path2 `shouldBe` ["COM", "b"]
            it "part2" $ do
                let m =
                        parseReverseOrbits
                            "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
                minTransfers "YOU" "SAN" m `shouldBe` 4
