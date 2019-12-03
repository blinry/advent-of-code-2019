module Day03Spec where

import Common
import Day03 hiding (main)
import Linear
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "all" $ do
                parseDirection "R8" `shouldBe` V2 8 0
                parseDirection "U5" `shouldBe` V2 0 (-5)
                parseWire "R8,U5" `shouldBe` [V2 8 0, V2 0 (-5)]
                points [] `shouldBe` [V2 0 0]
                points [V2 1 0] `shouldBe` [V2 0 0, V2 1 0]
                points [V2 1 0, V2 0 2] `shouldBe`
                    [V2 0 0, V2 1 0, V2 1 1, V2 1 2]
                crossings
                    [ points $ parseWire "R8,U5,L5,D3"
                    , points $ parseWire "U7,R6,D4,L4"
                    ] `shouldBe`
                    [V2 3 (-3), V2 6 (-5)]
                manhattan (V2 3 (-4)) `shouldBe` 7
                closest
                    (crossings
                         [ points $ parseWire "R8,U5,L5,D3"
                         , points $ parseWire "U7,R6,D4,L4"
                         ]) `shouldBe`
                    V2 3 (-3)
