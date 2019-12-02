module Day02Spec where

import Common
import Day02 hiding (main)
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "all" $ do
                step ([], [1, 0, 0, 0, 99]) `shouldBe` ([2, 0, 0, 0], [99])
                step ([2, 0, 0, 0], [99]) `shouldBe` ([2, 0, 0, 0, 99], [])
                step ([], [2, 3, 0, 3, 99]) `shouldBe` ([2, 3, 0, 6], [99])
                run [1, 1, 1, 4, 99, 5, 6, 0, 99] `shouldBe`
                    [30, 1, 1, 4, 2, 5, 6, 0, 99]
            it "get and put" $ do
                get ([4, 3, 2, 1], [6, 7, 8, 9]) 0 `shouldBe` 4
                get ([4, 3, 2, 1], [6, 7, 8, 9]) 3 `shouldBe` 1
                get ([4, 3, 2, 1], [6, 7, 8, 9]) 4 `shouldBe` 6
                put' [4, 3, 2, 1] 0 42 `shouldBe` [42, 3, 2, 1]
                put ([4, 3, 2, 1], [6, 7, 8, 9]) 0 42 `shouldBe`
                    ([42, 3, 2, 1], [6, 7, 8, 9])
                put ([4, 3, 2, 1], [6, 7, 8, 9]) 4 42 `shouldBe`
                    ([4, 3, 2, 1], [42, 7, 8, 9])
