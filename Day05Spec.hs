module Day05Spec where

import Common
import Day05 hiding (main)
import Test.Hspec

main =
    hspec $ do
        describe "computer" $
                --run [3, 0, 4, 0, 99] 42 `shouldBe` [42]
         do
            it "stops" $ do
                let c = fromList [99]
                let c' = step c
                running c' `shouldBe` False
            it "adds" $ do
                let c = fromList [1, 2, 3, 0, 99]
                let c' = step c
                pos c' `shouldBe` 4
                running c' `shouldBe` True
                memory c' 0 `shouldBe` 5
            it "multiplies" $ do
                let c = fromList [2, 2, 3, 0, 99]
                let c' = step c
                pos c' `shouldBe` 4
                running c' `shouldBe` True
                memory c' 0 `shouldBe` 6
            it "runs" $ do
                let c = fromList [2, 2, 3, 0, 99]
                run c [] `shouldBe` []
                run c [1] `shouldBe` []
            it "ios" $ do
                let c = fromList [3, 0, 4, 0, 99]
                run c [42] `shouldBe` [42]
