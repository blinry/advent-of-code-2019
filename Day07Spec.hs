module Day07Spec where

import Common
import qualified Data.Sequence as Seq
import Day05 hiding (main)
import Day07 hiding (main)
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "part1" $ do
                let c =
                        fromList
                            [ 3
                            , 15
                            , 3
                            , 16
                            , 1002
                            , 16
                            , 10
                            , 16
                            , 1
                            , 16
                            , 15
                            , 15
                            , 4
                            , 15
                            , 99
                            , 0
                            , 0
                            ]
                tryPhaseSetting c [4, 3, 2, 1, 0] `shouldBe` 43210
                let c2 =
                        fromList
                            [ 3
                            , 31
                            , 3
                            , 32
                            , 1002
                            , 32
                            , 10
                            , 32
                            , 1001
                            , 31
                            , -2
                            , 31
                            , 1007
                            , 31
                            , 0
                            , 33
                            , 1002
                            , 33
                            , 7
                            , 33
                            , 1
                            , 33
                            , 31
                            , 31
                            , 1
                            , 32
                            , 31
                            , 31
                            , 4
                            , 31
                            , 99
                            , 0
                            , 0
                            , 0
                            ]
                tryPhaseSetting c2 [1, 0, 4, 3, 2] `shouldBe` 65210
                findMaxOutput c2 `shouldBe` 65210
            it "nextOutput" $ do
                let c = fromList [3, 0, 4, 0, 4, 6, 99]
                let (c', out) = nextOutput 23 c
                out `shouldBe` Just 23
                let (c'', out2) = nextOutput 23 c'
                out2 `shouldBe` Just 99
                let (c''', out3) = nextOutput 23 c''
                out3 `shouldBe` Nothing
            it "part2" $ do
                let c =
                        fromList
                            [ 3
                            , 26
                            , 1001
                            , 26
                            , -4
                            , 26
                            , 3
                            , 27
                            , 1002
                            , 27
                            , 2
                            , 27
                            , 1
                            , 27
                            , 26
                            , 27
                            , 4
                            , 27
                            , 1001
                            , 28
                            , -1
                            , 28
                            , 1005
                            , 28
                            , 6
                            , 99
                            , 0
                            , 0
                            , 5
                            ]
                tryPhaseSetting2 c [9, 8, 7, 6, 5] `shouldBe` 139629729
                findMaxOutput2 c `shouldBe` 139629729
