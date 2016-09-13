import Data.List
import Data.Ord

--
data Classifier = Classifier ClassifierType LabelType

--
data ClassifierType = KNN {
                k :: Int,
                norm :: Norm
               }  | SVM

--
data LabelType = DoubleType | IntType deriving Show

--
data Label = LDouble Double | LInt Int deriving Show

--
type ExampleType = [Double]

--
type Norm = [Double] -> Double

--
data Loss = RegressionLoss (Double -> Double-> Double) | ClassificationLoss (Int -> Int -> Int)

--
data TrainingKnowledge = KNNKnowledge [ExampleType] [Label] Classifier | SVMKnowledge

--
instance Ord Label where
  (LDouble x) `compare` (LDouble y) = x `compare` y
  (LInt x) `compare` (LInt y) = x `compare` y
  _ `compare` _ = Prelude.error "No"

instance Eq Label where
    (==) (LDouble x) (LDouble y) = x == y
    (==) (LInt x) (LInt y) = x == y
    (/=) (LDouble x) (LDouble y) = x /= y
    (/=) (LInt x) (LInt y) = x /= y

train :: Classifier -> [ExampleType] -> [Label] -> TrainingKnowledge
train classifier@(Classifier (KNN k norm)  labelType) xs ys = KNNKnowledge xs ys classifier

--
classify :: TrainingKnowledge -> ExampleType -> Label
classify knowledge@(KNNKnowledge x y classifier@(Classifier (KNN k norm) DoubleType)) toClassify =
  LDouble (sum (map labelToDouble $ labelOfClosestNeighbors knowledge toClassify) / fromIntegral k)

classify knowledge@(KNNKnowledge x y classifier@(Classifier (KNN k norm) IntType)) toClassify = LInt result
  where
    result = snd $ maximumBy (comparing fst) $ zip (map length labelesGroupedByValue) [head x | x <- labelesGroupedByValue]
    labelesGroupedByValue = group (map labelToInt $ labelOfClosestNeighbors knowledge toClassify)

--
labelToDouble :: Label -> Double
labelToDouble (LDouble a) = a
labelToDouble (LInt a) = Prelude.error "Bad"

--
labelToInt :: Label -> Int
labelToInt (LInt a) = a
labelToInt (LDouble a) = Prelude.error "Bad"

--
labelOfClosestNeighbors :: TrainingKnowledge -> ExampleType -> [Label]
labelOfClosestNeighbors (KNNKnowledge x y (Classifier (KNN k norm) _)) toClassify =
    snd $ unzip $ take k $ sort $ zip (distanceFromVector norm x toClassify) y

--
distanceFromVector :: Norm -> [[Double]] -> [Double] -> [Double]
distanceFromVector norm x toClassify = map (norm . zipWith (-) toClassify) x

--
error :: TrainingKnowledge -> [ExampleType] -> [Label] -> Double
error knowledge@(KNNKnowledge x y classifier@(Classifier (KNN k norm) IntType)) xs ys =
    sum [binaryLoss curY_hat curY | (curY_hat, curY) <- zip y_hat ys] / fromIntegral (length ys)
  where
    y_hat = map (classify knowledge) xs

--
lNorm :: Double -> [Double] -> Double
lNorm n x = sum (map (** n) x) ** (1 / n)

--
binaryLoss :: (Eq a) => a -> a -> Double
binaryLoss x y = if x /= y then 1 else 0

--
l2Loss :: [Double] -> [Double] -> Double
l2Loss x y = lNorm 2 (zipWith (-) x y)
