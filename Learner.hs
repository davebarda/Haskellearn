module Learner(TrainingKnowledge, LearningParameters(..), LearningParametersType(..),
    train, classify, Learner.error) where

import Data.List
import Data.Ord
import Label
import Loss
import VectorUtils

--
data LearningParameters = LearningParameters LearningParametersType LabelType

--
data LearningParametersType = KNN {
                k :: Int,
                norm :: Norm
               }

--
type ExampleType = [Double]

--
data TrainingKnowledge = KNNKnowledge [ExampleType] [Label] LearningParameters

--
train :: LearningParameters -> [ExampleType] -> [Label] -> TrainingKnowledge
train classifier@(LearningParameters (KNN _  _)  _) xs ys = KNNKnowledge xs ys classifier

--
classify :: TrainingKnowledge -> ExampleType -> Label
classify knowledge@(KNNKnowledge _ _ (LearningParameters knn DoubleType)) toClassify =
  LDouble (sum (map labelToDouble $ labelOfClosestNeighbors knowledge toClassify) / fromIntegral (k knn))

classify knowledge@(KNNKnowledge _ _ (LearningParameters _ IntType)) toClassify = LInt result
  where
    result = snd $ maximumBy (comparing fst) $ zip (map length labelesGroupedByValue) labelsValues
    labelsValues = [head x | x <- labelesGroupedByValue]
    labelesGroupedByValue = group (map labelToInt $ labelOfClosestNeighbors knowledge toClassify)

--classify _ _ = Prelude.error "Classify doesn't support these types"

--
labelOfClosestNeighbors :: TrainingKnowledge -> ExampleType -> [Label]
labelOfClosestNeighbors (KNNKnowledge x y (LearningParameters knn _)) toClassify =
    snd $ unzip $ take (k knn) $ sort $ zip (distanceFromVector (norm knn) x toClassify) y

--
error :: TrainingKnowledge -> Loss -> [ExampleType] -> [Label] -> Double
error knowledge@(KNNKnowledge _ _ (LearningParameters _ IntType)) loss xs ys =
    sum [loss curY_hat curY | (curY_hat, curY) <- zip y_hat ys] / fromIntegral (length ys)
  where
    y_hat = map (classify knowledge) xs
error _ _ _ _ = Prelude.error "Training knowledge is not supported"
