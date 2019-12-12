module Day12Spec where

import Common
import Day12 hiding (main)
import Linear.V3
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "all" $ do
                parseMoon "<x=-17, y=9, z=-5>" `shouldBe`
                    Moon {pos = V3 (-17) 9 (-5), vel = V3 0 0 0}
