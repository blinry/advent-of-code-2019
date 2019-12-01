module Day00Spec where

import Common
import Day00 hiding (main)
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "all" $ do
                True `shouldBe` True
                False `shouldBe` False
