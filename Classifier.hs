import Data.List

--
data Classifier = KNN {
                k :: Int,
                norm :: Norm
               }  | SVM

--
type Norm = [Double] -> Double

--
data TrainingKnowledge = KNNKnowledge [[Double]] [Double] Int | SVMKnowledge deriving Show

--
data Classification = KNNRegression Double | KNNClassification Int | SVMClassification deriving Show

--
train :: Classifier -> [[Double]] -> [Double] -> TrainingKnowledge
train (KNN k norm) xs ys = KNNKnowledge xs ys (length ys)

--
classify :: Classifier -> TrainingKnowledge -> [Double] -> Classification
classify classifier@(KNN k norm) knowledge@(KNNKnowledge x y len) toClassify = sum (orderByDistance classifier knowledge toClassify) / fromIntegral len

--
orderByDistance :: Classifier -> TrainingKnowledge -> [Double] -> [Double]
orderByDistance (KNN k norm) (KNNKnowledge x y _) toClassify = snd $ unzip $ take k $ sort $ zip (distanceFromVector norm x toClassify) y

--
distanceFromVector :: Norm ->  [[Double]] -> [Double] -> [Double]
distanceFromVector norm x toClassify = map (norm . zipWith (-) toClassify) x

--
error :: Classifier -> TrainingKnowledge -> [[Double]] -> [Double] -> Double
error (KNN k norm) knowledge xs ys = sum [(binaryLoss curY_hat curY) | (curY_hat, curY) <- zip y_hat ys] / (fromIntegral (length ys))
  where
    y_hat = map (classify (KNN k norm) knowledge) xs

--
lNorm :: Double -> [Double] -> Double
lNorm n x = sum (map (** n) x) ** (1 / n)

--
binaryLoss :: (Eq a) => a -> a -> Double
binaryLoss x y = if x /= y then 1.0 else 0.0
