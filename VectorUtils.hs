module VectorUtils(distanceFromVector, lNorm, Norm) where

type Norm = [Double] -> Double

distanceFromVector :: Norm -> [[Double]] -> [Double] -> [Double]
distanceFromVector norm x toClassify = map (norm . zipWith (-) toClassify) x

--
lNorm :: Double -> [Double] -> Double
lNorm n x = sum (map (** n) x) ** (1 / n)
