module EllipsoidSpec where

import Test.Hspec
import OnlineLearner
import Label
import Data.Matrix

main :: IO ()
main = hspec $ do
  let knowledge = initKnowledge (EllipsoidParameters 2)
  let eta = 4 / 3

  let initialKnowledge = EllipsoidKnowledge 2 eta (identity 2) (zero 1 2)
  let train1Knowledge = EllipsoidKnowledge 2 eta (fromList 2 2 [4 / 9, 0, 0, 4 / 3]) (fromList 1 2 [1 / 3, 0])

  let x1 = fromList 2 1 [1, 0]
  let y1 = LInt 1

  describe "Init tests" $
    it "Initialization test" $
      knowledge `shouldBe` initialKnowledge

  describe "Train tests" $ do
    let knowTrain = train knowledge x1 y1

    it "Train on one example" $
      knowTrain `shouldBe` train1Knowledge

  describe "Batch tests" $
    it "Train on one example" $ do
      let batchKnowledge = batch knowledge [x1] [y1]
      batchKnowledge `shouldBe` train1Knowledge
