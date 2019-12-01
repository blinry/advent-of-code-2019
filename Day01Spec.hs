module Day01Spec where

import Common
import Day01 hiding (main)
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "all" $ do
                fuel 1969 `shouldBe` 654
                fuel 100756 `shouldBe` 33583
