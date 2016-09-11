{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

module Classifier
(
    Classifier,
    train,
    classify,
    Classifier.error
) where

class Classifier classifier knowledge x y where
  train :: classifier -> [x] -> [y] -> knowledge
  classify :: classifier -> knowledge -> x -> y
  error :: classifier -> knowledge -> [x] -> [y] -> Float
