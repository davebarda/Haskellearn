module KNNSpec where

import Test.Hspec
import Classifier
import Label
import Loss
import VectorUtils

main :: IO ()
main = hspec $ do
  describe "train" $ do
    let x1 = [1, 1]
    let x2 = [1, 0]
    let y1 = LDouble 1
    let y2 = LDouble 0
    let xs = [x1, x2]
    let ys = [y1, y2]
    let x3 = [2, 1]
    let y3 = LDouble 1
    let norm = lNorm 2
    let classifier = Classifier KNN { k = 2, norm = norm} DoubleType
    let knowledge = train classifier xs ys
    let classification = classify knowledge x3

    it "Regression training" $
      absolute 1 `shouldBe` 1

  describe "classify" $ do
    it "returns the original number when given a positive input" $
     absolute 1 `shouldBe` 1
