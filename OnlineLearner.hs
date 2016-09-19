module OnlineLearner(TrainingKnowledge(..), LearnerParameters(..),
    initKnowledge, train, classify, batch) where

import Label
import Writer
import Data.Matrix

--
type Example = Matrix Double

--
data LearnerParameters = EllipsoidParameters Int deriving Show

--
data TrainingKnowledge = EllipsoidKnowledge { dimension :: Int, eta :: Double, a :: Matrix Double, w:: Matrix Double } deriving (Show, Eq)

--
initKnowledge :: LearnerParameters -> Writer (DiffList Char) TrainingKnowledge
initKnowledge (EllipsoidParameters d) = do
    let dDouble = fromInteger $ toInteger d
    let eta' =  dDouble * dDouble / (dDouble * dDouble - 1) :: Double
    let a' = identity d
    let w' = zero 1 d
    let res = EllipsoidKnowledge d eta' a' w'
    tell $ toDiffList $ "Called initKnoweledge with dimension: " ++ show d ++ " and result was: " ++ show res
    return res

--
train :: TrainingKnowledge -> Example -> Label -> TrainingKnowledge
train knowledge@(EllipsoidKnowledge d eta' a' w') example (LInt trueY) =
  if yhat /= trueY then newKnowledge else knowledge
  where
    yhat = labelToInt $ classify knowledge example
    ax = a' * example
    xax = (transpose example * ax) ! (1, 1)
    dDouble = fromInteger $ toInteger d
    newW = w' + scaleMatrix (fromIntegral trueY / ((dDouble + 1) * sqrt xax)) (transpose ax)
    newA = scaleMatrix eta' (a' - scaleMatrix (2 / ((dDouble + 1) * xax))  ax * transpose ax)
    newKnowledge = EllipsoidKnowledge d eta' newA newW

train _ _ _ = Prelude.error "Train doesn't support these types"

--
classify :: TrainingKnowledge -> Example -> Label
classify (EllipsoidKnowledge _ _ _ w') example = LInt $ round (signum (w'  * example) ! (1, 1))

--
batch :: TrainingKnowledge -> [Example] -> [Label] -> TrainingKnowledge
batch knowledge [] [] = knowledge
batch knowledge examples@(x:xs) labels@(y:ys) =
  if correctLength then batch (train knowledge x y) xs ys else errorReturn
  where
    correctLength = length examples == length labels
    errorReturn = Prelude.error "Examples length mismatch labels length"
batch _ _ _ = Prelude.error "batch doesn't support such input"
