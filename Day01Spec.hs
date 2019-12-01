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
                fuel 9 `shouldBe` 1
                fuel 8 `shouldBe` 0
                fuel 7 `shouldBe` 0
                fuel 6 `shouldBe` 0
                fuel 5 `shouldBe` 0
                totalFuel 1969 `shouldBe` 966
                totalFuel 100756 `shouldBe` 50346
