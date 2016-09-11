{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

module KNN
(
    KNN
) where

import Classifier
import Data.List

--
newtype KNNInput = KNNInput [Double]
newtype KNNLabel = KNNLabel Double
newtype KNNknowledge = KNNknowledge ([KNNInput], [KNNLabel], Int)
type KNNNorm = KNNInput -> KNNInput -> Double

data KNN = KNN {
                k :: Int,
                norm :: KNNNorm
               }
--
instance Classifier KNN KNNknowledge KNNInput KNNLabel where
  train (KNN k norm) x y = KNNknowledge (x, y, length y)
  classify (KNN k norm) (KNNknowledge (x, y, len)) toClassify = KNNLabel 0 --(sum $ orderByDistance (KNNknowledge (x, y, len)) toClassify `div` len)
  error (KNN k norm) knowledge x y = sum [binaryLoss curY_hat curY | (curY_hat, curY) <- zip y_hat y] `div` fromIntegral (length y)
    where
      y_hat = map (classify (KNN k norm) knowledge) x

orderByDistance :: KNN -> KNNknowledge -> KNNInput -> [KNNLabel]
orderByDistance (KNN k norm) (KNNknowledge (x, y, _)) toClassify = snd $ unzip $ take k $ sort $ zip (distanceFromVector norm x toClassify) y

distanceFromVector :: KNNNorm ->  [KNNInput] -> KNNInput -> KNNInput
distanceFromVector norm x toClassify = map (norm 2 $ - toClassify) x

lNorm :: Int -> KNNNorm
lNorm n x = sum (map (** n) x) ** (1 / n)

binaryLoss :: (Eq a) => a -> a -> Float
binaryLoss x y = if x /= y then 1 else 0
