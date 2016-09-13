import Data.List
import Data.Ord

--
data Classifier = Classifier ClassifierType LabelType

--
data ClassifierType = KNN {
                k :: Int,
                norm :: Norm
                -- , loss :: Loss
               }  | SVM

--
data LabelType = LDouble Double | LInt Int deriving Show

instance Ord LabelType where
  (LDouble x) `compare` (LDouble y) = x `compare` y
  (LInt x) `compare` (LInt y) = x `compare` y
  _ `compare` _ = Prelude.error "No"

instance Eq LabelType where
    (==) (LDouble x) (LDouble y) = x == y
    (==) (LInt x) (LInt y) = x == y
    (/=) (LDouble x) (LDouble y) = x /= y
    (/=) (LInt x) (LInt y) = x /= y

--
type ExampleType = [Double]

--
type Norm = [Double] -> Double

--
data TrainingKnowledge = KNNKnowledge [ExampleType] [LabelType] Int | SVMKnowledge deriving Show

--
train :: Classifier -> [ExampleType] -> [LabelType] -> TrainingKnowledge
train (Classifier (KNN _ _)  _) xs ys = KNNKnowledge xs ys (length ys)

--
classify :: Classifier -> TrainingKnowledge -> ExampleType -> LabelType
classify classifier@(Classifier (KNN k norm) (LDouble _)) knowledge@(KNNKnowledge x y len) toClassify = LDouble result
  where
    result = sum (map labelTypeToDouble $ labelOfClosestNeighbors classifier knowledge toClassify) / fromIntegral len

classify classifier@(Classifier (KNN k norm) (LInt _)) knowledge@(KNNKnowledge x y len) toClassify = LInt result
  where
    result = snd $ maximumBy (comparing fst) $ zip (map length labelesGroupedByValue) [head x | x <- labelesGroupedByValue]
    labelesGroupedByValue = group (map labelTypeToInt $ labelOfClosestNeighbors classifier knowledge toClassify)

--
labelTypeToDouble :: LabelType -> Double
labelTypeToDouble (LDouble a) = a
labelTypeToDouble (LInt a) = Prelude.error "Bad"

--
labelTypeToInt :: LabelType -> Int
labelTypeTopeToInt (LInt a) = a
labelTypeToInt (LDouble a) = Prelude.error "Bad"

--
labelOfClosestNeighbors :: Classifier -> TrainingKnowledge -> ExampleType -> [LabelType]
labelOfClosestNeighbors (Classifier (KNN k norm)  _) (KNNKnowledge x y _) toClassify =
  snd $ unzip $ take k $ sort $ zip (distanceFromVector norm x toClassify) y

--
distanceFromVector :: Norm ->  [[Double]] -> [Double] -> [Double]
distanceFromVector norm x toClassify = map (norm . zipWith (-) toClassify) x

--
error :: Classifier -> TrainingKnowledge -> [ExampleType] -> [LabelType] -> Double
error (Classifier (KNN k norm)  labelType) knowledge xs ys =
    sum [binaryLoss curY_hat curY | (curY_hat, curY) <- zip y_hat ys] / fromIntegral (length ys)
  where
    y_hat = map (classify (Classifier (KNN k norm)  labelType) knowledge) xs

--
lNorm :: Double -> [Double] -> Double
lNorm n x = sum (map (** n) x) ** (1 / n)

--
binaryLoss :: (Eq a) => a -> a -> Double
binaryLoss x y = if x /= y then 1.0 else 0.0
