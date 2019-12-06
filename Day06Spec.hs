module Day00Spec where

import Common
import qualified Data.Map as M
import Data.Map (Map)
import Day06 hiding (main)
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "all" $ do
                parseOrbits "a)b" `shouldBe` M.fromList [("a", ["b"])]
                parseOrbits "a)b\nb)c\na)c" `shouldBe`
                    M.fromList [("a", ["c", "b"]), ("b", ["c"])]
                orbits (parseOrbits "COM)a") `shouldBe` 1
                orbits (parseOrbits "COM)a\nCOM)b") `shouldBe` 2
                orbits (parseOrbits "COM)a\nCOM)b\nb)c\nb)d") `shouldBe` 6
                orbits
                    (parseOrbits
                         "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") `shouldBe`
                    42
