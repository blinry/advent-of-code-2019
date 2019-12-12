module Day11Spec where

import Common
import Day11 hiding (main)
import Linear.V2
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "all" $ do
                doTurn (V2 0 1) 0 `shouldBe` V2 1 0
                False `shouldBe` False
