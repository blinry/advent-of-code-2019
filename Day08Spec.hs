module Day08Spec where

import Common
import Day08 hiding (main)
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "all" $ do
                True `shouldBe` True
                False `shouldBe` False
