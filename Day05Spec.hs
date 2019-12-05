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
                let c = fromList [1, 0, 4, 0, 99]
                let c' = step c
                pos c' `shouldBe` 4
                running c' `shouldBe` True
                memory c' 0 `shouldBe` 100
            it "multiplies" $ do
                let c = fromList [2, 0, 4, 0, 99]
                let c' = step c
                pos c' `shouldBe` 4
                running c' `shouldBe` True
                memory c' 0 `shouldBe` 198
            it "runs" $ do
                let c = fromList [2, 2, 3, 0, 99]
                run c [] `shouldBe` []
                run c [1] `shouldBe` []
            it "ios" $ do
                let c = fromList [3, 0, 4, 0, 99]
                run c [42] `shouldBe` [42]
            it "adds in immediate mode" $ do
                let c = fromList [1101, 2, 3, 0, 99]
                let c' = step c
                memory c' 0 `shouldBe` 5
            it "multiplies in mixed mode" $ do
                let c = fromList [102, 2, 4, 0, 99]
                let c' = step c
                memory c' 0 `shouldBe` 198
        describe "op" $ do
            it "can be parsed" $ do
                opParse 101 `shouldBe` (1, [1])
                opParse 101105 `shouldBe` (5, [1, 1, 0, 1])
