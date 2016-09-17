module KNNSpec where

import Test.Hspec
import Learner
import Label
import VectorUtils

main :: IO ()
main = hspec $ do
  let x1 = [1, 1]
  let x2 = [1, 0]
  let x3 = [2, 1]

  describe "Regression" $ do
    let classifier = Classifier KNN {k = 2, norm = lNorm 2} DoubleType
    let knowledge = train classifier [x1, x2] [LDouble 1, LDouble 0]
    let classification = classify knowledge x3

    it "Regression test" $
      classification `shouldBe` LDouble 0.5

  describe "Classification" $ do
    let classifier = Classifier KNN {k = 1, norm = lNorm 2} IntType
    let knowledge = train classifier [x1, x2] [LInt 1, LInt 0]
    let classification = classify knowledge x3

    it "Classification test" $
      classification `shouldBe` LInt 1
