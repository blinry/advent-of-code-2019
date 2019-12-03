module Day03Spec where

import Common
import Data.Map as Map
import Data.Set as Set
import Day03 hiding (main)
import Linear
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "all" $ do
                points [V2 1 0, V2 0 2] `shouldBe`
                    Set.fromList ([V2 1 0, V2 1 1, V2 1 2])
                points2 [V2 1 0, V2 0 2] `shouldBe`
                    Map.fromList ([(V2 1 0, 1), (V2 1 1, 2), (V2 1 2, 3)])
