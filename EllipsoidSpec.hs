module EllipsoidSpec where

import Label
import Writer
import Test.Hspec
import OnlineLearner
import Data.Matrix
--
roundKnowledgeToNDigits :: Int -> TrainingKnowledge -> TrainingKnowledge
roundKnowledgeToNDigits digits (EllipsoidKnowledge d eta a w) = EllipsoidKnowledge d roundedEta roundedA roundedW
  where
    roundToNDigits n num =  fromInteger (round $ num * (10 ^ n)) / (10.0 ^^ n)
    roundMapToNDigits n = fmap (roundToNDigits n)
    roundedEta = roundToNDigits digits eta
    roundedA = roundMapToNDigits digits a
    roundedW = roundMapToNDigits digits w

--
roundAndCompare :: TrainingKnowledge -> TrainingKnowledge -> Expectation
roundAndCompare k1 k2 = roundKnowledgeToNDigits precision k1 `shouldBe` roundKnowledgeToNDigits precision k2
  where precision = 9

--
main :: IO ()
main = do
    let Writer (initialKnowledge, initLog) = initKnowledge (EllipsoidParameters 2)

    let eta' = 4 / 3
    let train1Knowledge = EllipsoidKnowledge 2 eta' (fromList 2 2 [4 / 9, 0, 0, 4 / 3]) (fromList 1 2 [1 / 3, 0])
    let x1 = fromList 2 1 [1, 0]
    let y1 = LInt 1

    let Writer (trainKnowledge, trainLog) = train initialKnowledge x1 y1

    hspec $ do
      describe "Init tests" $
        it "Initialization test" $
          roundAndCompare initialKnowledge (EllipsoidKnowledge 2 eta' (identity 2) (zero 1 2))

      describe "Train tests" $
        it "Train on one example" $
          roundAndCompare trainKnowledge train1Knowledge

      describe "Batch tests" $
        it "Train on one example" $
          roundAndCompare (batch initialKnowledge [x1] [y1]) train1Knowledge

    putStrLn "------Done Testing------\nCallStack log:"
    putStrLn $ fromDiffList initLog
    putStrLn $ fromDiffList trainLog
