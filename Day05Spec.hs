module Day05Spec where

import Common
import qualified Data.Sequence as Seq
import Day05 hiding (main)
import Test.Hspec

main =
    hspec $ do
        describe "computer" $ do
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
                run [] c `shouldBe` []
                run [1] c `shouldBe` []
            it "inputs" $ do
                let c = fromList [3, 0, 99]
                let c' = c {input = Seq.fromList [123]}
                let c'' = step c'
                memory c'' 0 `shouldBe` 123
            it "outputs" $ do
                let c = fromList [4, 2, 99]
                run [] c `shouldBe` [99]
            it "ios" $ do
                let c = fromList [3, 0, 4, 0, 99]
                run [23] c `shouldBe` [23]
            it "adds in immediate mode" $ do
                let c = fromList [1101, 2, 3, 0, 99]
                let c' = step c
                memory c' 0 `shouldBe` 5
            it "multiplies in mixed mode" $ do
                let c = fromList [102, 2, 4, 0, 99]
                let c' = step c
                memory c' 0 `shouldBe` 198
            it "outputs in immediate mode" $ do
                let c = fromList [104, 2, 99]
                run [] c `shouldBe` [2]
        describe "op" $ do
            it "can be parsed" $ do
                opParse 101 `shouldBe` (1, [1])
                opParse 101105 `shouldBe` (5, [1, 1, 0, 1])
