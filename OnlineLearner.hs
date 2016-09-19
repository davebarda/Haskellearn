module OnlineLearner(TrainingKnowledge(..), LearnerParameters(..),
    initKnowledge, train, classify, batch) where

import Label
import Data.Matrix

--
type Example = Matrix Double

--
data LearnerParameters = EllipsoidParameters Int deriving Show

--
data TrainingKnowledge = EllipsoidKnowledge Int Double (Matrix Double) (Matrix Double)
  deriving (Show, Eq)

--
initKnowledge :: LearnerParameters -> TrainingKnowledge
initKnowledge (EllipsoidParameters d) = EllipsoidKnowledge d eta a w
  where
    dDouble = fromInteger $ toInteger d
    eta = dDouble * dDouble / (dDouble * dDouble - 1)
    a = identity d
    w = zero 1 d

--
train :: TrainingKnowledge -> Example -> Label -> TrainingKnowledge
train knowledge@(EllipsoidKnowledge d eta a w) example (LInt trueY) =
  if yhat /= trueY then newKnowledge else knowledge
  where
    yhat = labelToInt $ classify knowledge example
    ax = a * example
    xax = (transpose example * ax) ! (1, 1)
    dDouble = fromInteger $ toInteger d
    newW = w + scaleMatrix (fromIntegral trueY / ((dDouble + 1) * sqrt xax)) (transpose ax)
    newA = scaleMatrix eta (a - scaleMatrix (2 / ((dDouble + 1) * xax))  ax * transpose ax)
    newKnowledge = EllipsoidKnowledge d eta newA newW

train _ _ _ = Prelude.error "Train doesn't support these types"

--
classify :: TrainingKnowledge -> Example -> Label
classify (EllipsoidKnowledge _ _ _ w) example = LInt $ round (signum (w  * example) ! (1, 1))

--
batch :: TrainingKnowledge -> [Example] -> [Label] -> TrainingKnowledge
batch knowledge [] _ = knowledge
batch knowledge _ [] = knowledge
batch knowledge examples@(x:xs) labels@(y:ys) =
  if correctLength then batch (train knowledge x y) xs ys else errorReturn
  where
    correctLength = length examples == length labels
    errorReturn = Prelude.error "Examples length mismatch labels length"
