module KNNSpec where
-- This module that contains tests for the KNN learner

import Test.Hspec
import Learner
import Label
import VectorUtils

-- Run all the tests
main :: IO ()
main = hspec $ do
  let x1 = [1, 1]
  let x2 = [1, 2]
  let x3 = [1, 3]
  let x4 = [1,4]
  let x5 = [1,5]
  let x6 = [1,0]

  describe "Regression" $ do
    let classifier = LearningParameters KNN {k = 2, norm = lNorm 2} DoubleType
    let knowledge = train classifier [x1, x2] [LDouble 1, LDouble 0]
    let classification = classify knowledge x3

    it "Regression test" $
      classification `shouldBe` LDouble 0.5

    let classifier' = LearningParameters KNN {k = 4, norm = lNorm 2} DoubleType
    let knowledge' = train classifier' [x1, x2, x3, x4, x5] [LDouble 1, LDouble 1, LDouble 1, LDouble 0, LDouble 1]
    let classification' = classify knowledge' x3

    it "Regression complicated test" $
      classification' `shouldBe` LDouble (3/4)

  describe "Classification" $ do
    let classifier = LearningParameters KNN {k = 1, norm = lNorm 2} IntType
    let knowledge = train classifier [x1, x2] [LInt 1, LInt 0]
    let classification = classify knowledge x3

    it "Classification test" $
        classification `shouldBe` LInt 0

    let classifier' = LearningParameters KNN {k = 4, norm = lNorm 2} IntType
    let knowledge' = train classifier' [x1, x2, x3, x4, x5] [LInt 1, LInt 0, LInt 1, LInt 0, LInt 1]
    let classification' = classify knowledge' x6
    it "Classification complicated test" $
        classification' `shouldBe` LInt 0

    let classifier'' = LearningParameters KNN {k = 5, norm = lNorm 2} IntType
    let knowledge'' = train classifier'' [x1, x2, x3, x4, x5] [LInt 1, LInt 0, LInt 1, LInt 0, LInt 1]
    let classification'' = classify knowledge'' x6

    it "Classification complicated test 2" $
      classification'' `shouldBe` LInt 1
