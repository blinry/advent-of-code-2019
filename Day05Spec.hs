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
            it "jumps-if-true" $ do
                let c = fromList [105, 1, 2, 99]
                let c' = step c
                pos c' `shouldBe` 2
            it "jumps-if-false" $ do
                let c = fromList [106, 0, 2, 99]
                let c' = step c
                pos c' `shouldBe` 2
            it "lesses than" $ do
                let c = fromList [1107, 1, 2, 0, 99]
                let c' = step c
                memory c' 0 `shouldBe` 1
            it "not lesses than" $ do
                let c = fromList [1107, 2, 2, 0, 99]
                let c' = step c
                memory c' 0 `shouldBe` 0
            it "equals" $ do
                let c = fromList [1108, 2, 2, 1, 99]
                let c' = step c
                memory c' 1 `shouldBe` 1
            it "not equals" $ do
                let c = fromList [1108, 1, 2, 1, 99]
                let c' = step c
                memory c' 1 `shouldBe` 0
            it "handles address not equals" $ do
                let c = fromList [8, 0, 4, 1, 99]
                let c' = step c
                memory c' 1 `shouldBe` 0
            it "handles address equals" $ do
                let c = fromList [8, 6, 6, 1, 99, 42, 42]
                let c' = step c
                memory c' 1 `shouldBe` 1
            it "handles small tests" $ do
                run [8] (fromList [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]) `shouldBe`
                    [1]
                run [9] (fromList [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]) `shouldBe`
                    [0]
                run [7] (fromList [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]) `shouldBe`
                    [1]
                run [8] (fromList [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]) `shouldBe`
                    [0]
                run [8] (fromList [3, 3, 1108, -1, 8, 3, 4, 3, 99]) `shouldBe`
                    [1]
                run [7] (fromList [3, 3, 1108, -1, 8, 3, 4, 3, 99]) `shouldBe`
                    [0]
                run [7] (fromList [3, 3, 1107, -1, 8, 3, 4, 3, 99]) `shouldBe`
                    [1]
                run [8] (fromList [3, 3, 1107, -1, 8, 3, 4, 3, 99]) `shouldBe`
                    [0]
            it "handles a large test" $ do
                let c =
                        fromList
                            [ 3
                            , 21
                            , 1008
                            , 21
                            , 8
                            , 20
                            , 1005
                            , 20
                            , 22
                            , 107
                            , 8
                            , 21
                            , 20
                            , 1006
                            , 20
                            , 31
                            , 1106
                            , 0
                            , 36
                            , 98
                            , 0
                            , 0
                            , 1002
                            , 21
                            , 125
                            , 20
                            , 4
                            , 20
                            , 1105
                            , 1
                            , 46
                            , 104
                            , 999
                            , 1105
                            , 1
                            , 46
                            , 1101
                            , 1000
                            , 1
                            , 20
                            , 4
                            , 20
                            , 1105
                            , 1
                            , 46
                            , 98
                            , 99
                            ]
                run [7] c `shouldBe` [999]
                run [8] c `shouldBe` [1000]
                run [9] c `shouldBe` [1001]
        describe "op" $ do
            it "can be parsed" $ do
                opParse 101 `shouldBe` (1, [1])
                opParse 101105 `shouldBe` (5, [1, 1, 0, 1])
