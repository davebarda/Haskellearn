import Data.List
import Data.Ord

--
data Classifier = Classifier ClassifierType Label

--
data ClassifierType = KNN {
                k :: Int,
                norm :: Norm
                -- , loss :: Loss
               }  | SVM

--
data Label = LDouble Double | LInt Int deriving Show

instance Ord Label where
  (LDouble x) `compare` (LDouble y) = x `compare` y
  (LInt x) `compare` (LInt y) = x `compare` y
  _ `compare` _ = Prelude.error "No"

instance Eq Label where
    (==) (LDouble x) (LDouble y) = x == y
    (==) (LInt x) (LInt y) = x == y
    (/=) (LDouble x) (LDouble y) = x /= y
    (/=) (LInt x) (LInt y) = x /= y

--
type ExampleType = [Double]

--
type Norm = [Double] -> Double

--
data TrainingKnowledge = KNNKnowledge [ExampleType] [Label] Int | SVMKnowledge deriving Show

--
train :: Classifier -> [ExampleType] -> [Label] -> TrainingKnowledge
train (Classifier (KNN _ _)  _) xs ys = KNNKnowledge xs ys (length ys)

--
classify :: Classifier -> TrainingKnowledge -> ExampleType -> Label
classify classifier@(Classifier (KNN k norm) (LDouble _)) knowledge@(KNNKnowledge x y len) toClassify = LDouble result
  where
    result = sum (map labelToDouble $ labelOfClosestNeighbors classifier knowledge toClassify) / fromIntegral len

classify classifier@(Classifier (KNN k norm) (LInt _)) knowledge@(KNNKnowledge x y len) toClassify = LInt result
  where
    result = snd $ maximumBy (comparing fst) $ zip (map length labelesGroupedByValue) [head x | x <- labelesGroupedByValue]
    labelesGroupedByValue = group (map labelToInt $ labelOfClosestNeighbors classifier knowledge toClassify)

--
labelToDouble :: Label -> Double
labelToDouble (LDouble a) = a
labelToDouble (LInt a) = Prelude.error "Bad"

--
labelToInt :: Label -> Int
labelToInt (LInt a) = a
labelToInt (LDouble a) = Prelude.error "Bad"

--
labelOfClosestNeighbors :: Classifier -> TrainingKnowledge -> ExampleType -> [Label]
labelOfClosestNeighbors (Classifier (KNN k norm)  _) (KNNKnowledge x y _) toClassify =
  snd $ unzip $ take k $ sort $ zip (distanceFromVector norm x toClassify) y

--
distanceFromVector :: Norm ->  [[Double]] -> [Double] -> [Double]
distanceFromVector norm x toClassify = map (norm . zipWith (-) toClassify) x

--
error :: Classifier -> TrainingKnowledge -> [ExampleType] -> [Label] -> Double
error classifier@(Classifier (KNN k norm)  label) knowledge xs ys =
    sum [binaryLoss curY_hat curY | (curY_hat, curY) <- zip y_hat ys] / fromIntegral (length ys)
  where
    y_hat = map (classify classifier knowledge) xs

--
lNorm :: Double -> [Double] -> Double
lNorm n x = sum (map (** n) x) ** (1 / n)

--
binaryLoss :: (Eq a) => a -> a -> Double
binaryLoss x y = if x /= y then 1.0 else 0.0
