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
    let res = EllipsoidKnowledge d eta' (identity d) (zero 1 d)
    tell $ toDiffList $ "Called initKnoweledge with dimension: " ++ show d ++ " and the result was: " ++ show res
    return res

--
train :: TrainingKnowledge -> Example -> Label -> Writer (DiffList Char) TrainingKnowledge
train knowledge@(EllipsoidKnowledge d eta' a' w') example (LInt trueY) = do
  let yhat = labelToInt $ getValFromWriter $ classify knowledge example
  tell $ toDiffList $ "Called train with EllipsoidKnowledge: " ++ show knowledge ++ ", the prediction " ++ show yhat
  if yhat == trueY
    then do
      tell $ toDiffList $ " was correct, so the result was: " ++ show knowledge
      return knowledge
    else do
      let ax = a' * example
      let xax = (transpose example * ax) ! (1, 1)
      let dDouble = fromInteger $ toInteger d
      let newW = w' + scaleMatrix (fromIntegral trueY / ((dDouble + 1) * sqrt xax)) (transpose ax)
      let newA = scaleMatrix eta' (a' - scaleMatrix (2 / ((dDouble + 1) * xax))  ax * transpose ax)
      let newKnowledge = EllipsoidKnowledge d eta' newA newW
      tell $ toDiffList $ " was wrong, so the result was: " ++ show newKnowledge
      return newKnowledge

train _ _ _ = Prelude.error "Train doesn't support these types"

--
classify :: TrainingKnowledge -> Example -> Writer (DiffList Char) Label
classify knowledge@(EllipsoidKnowledge _ _ _ w') example =  do
  let res = LInt $ round (signum (w'  * example) ! (1, 1))
  tell $ toDiffList $ "Called classify with EllipsoidKnowledge: " ++ show knowledge ++ " and example: " ++ show example ++ ", the prediction is" ++ show res
  return res

--
batch :: TrainingKnowledge -> [Example] -> [Label] -> TrainingKnowledge
batch knowledge [] [] = knowledge
batch knowledge examples@(x:xs) labels@(y:ys) =
  if correctLength then batch (getValFromWriter (train knowledge x y)) xs ys else errorReturn
  where
    correctLength = length examples == length labels
    errorReturn = Prelude.error "Examples length mismatch labels length"
batch _ _ _ = Prelude.error "batch doesn't support such input"
