module Day04Spec where

import Common
import Day04 hiding (main)
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "all" $ do
                True `shouldBe` True
                False `shouldBe` False
