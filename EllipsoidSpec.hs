module EllipsoidSpec where

import Test.Hspec
import OnlineLearner
import Label
import Data.Matrix

main :: IO ()
main = hspec $ do
  let know = initKnowledge (EllipsoidParameters 2)

  let eta = 4 / 3
  let x1 = fromList 2 1 [1, 0]
  let y1 = LInt 1

  describe "Init tests" $
    it "Initialization test" $
      know `shouldBe` EllipsoidKnowledge 2 eta (identity 2) (zero 1 2)

  describe "Train tests" $ do
    let knowTrain = train know x1 y1
    let correctW = fromList 1 2 [1 / 3, 0]
    let correctA  = fromList 2 2 [4 / 9, 0, 0, 4 / 3]

    it "Train on one example" $
      knowTrain `shouldBe` EllipsoidKnowledge 2 eta correctA correctW
