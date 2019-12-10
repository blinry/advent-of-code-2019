module Day10Spec where

import Common
import qualified Data.Set as S
import Data.Set (Set)
import Day10 hiding (main)
import Linear.V2
import Test.Hspec

main =
    hspec $ do
        describe "all" $ do
            it "all" $ do
                parseField "#" `shouldBe` S.fromList [V2 0 0]
                parseField "#.#" `shouldBe` S.fromList [V2 0 0, V2 2 0]
                parseField "#.\n..\n.#" `shouldBe` S.fromList [V2 0 0, V2 1 2]
                between (V2 0 0) (V2 3 0) `shouldBe` [V2 1 0, V2 2 0]
                between (V2 0 0) (V2 0 3) `shouldBe` [V2 0 1, V2 0 2]
                between (V2 0 0) (V2 3 (-9)) `shouldBe` [V2 1 (-3), V2 2 (-6)]
                between (V2 1 1) (V2 4 7) `shouldBe` [V2 2 3, V2 3 5]
            it "part2" $ do
                directions 2 `shouldBe`
                    [ V2 0 (-1)
                    , V2 1 (-1)
                    , V2 1 0
                    , V2 1 1
                    , V2 0 1
                    , V2 (-1) 1
                    , V2 (-1) 0
                    , V2 (-1) (-1)
                    ]
                hit (S.fromList [V2 0 0, V2 2 0, V2 5 0]) (V2 0 0) (V2 1 0) `shouldBe`
                    Just (V2 2 0)
                shoot (S.fromList [V2 0 0, V2 2 0, V2 5 0]) (V2 0 0) (V2 1 0) `shouldBe`
                    S.fromList [V2 0 0, V2 5 0]
                shoot (S.fromList [V2 0 0, V2 5 0]) (V2 0 0) (V2 1 0) `shouldBe`
                    S.fromList [V2 0 0]
                shoot (S.fromList [V2 0 0]) (V2 0 0) (V2 1 0) `shouldBe`
                    S.fromList [V2 0 0]
