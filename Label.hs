module Label(Label(..), LabelType(..), labelToDouble, labelToInt) where

-- LabelType data type represents the type of the labels that can be returned by
-- a learner
data LabelType = DoubleType | IntType deriving Show

-- Label datatype represents the value returned by a learner
data Label = LDouble Double | LInt Int deriving Show

instance Ord Label where
  (LDouble x) `compare` (LDouble y) = x `compare` y
  (LInt x) `compare` (LInt y) = x `compare` y
  _ `compare` _ = Prelude.error "Can't compare these types"

instance Eq Label where
  (==) (LDouble x) (LDouble y) = x == y
  (==) (LInt x) (LInt y) = x == y
  (==) _ _ = Prelude.error "Can't compare these types"
  (/=) (LDouble x) (LDouble y) = x /= y
  (/=) (LInt x) (LInt y) = x /= y
  (/=) _ _ =  Prelude.error "Can't compare these types"

-- A function that get the double value out of a label when it hold a double value.
labelToDouble :: Label -> Double
labelToDouble (LDouble a) = a
labelToDouble _ = Prelude.error "Label isn't a double"

-- A function that get the double value out of a label when it hold a int value.
labelToInt :: Label ->  Int
labelToInt (LInt a) = a
labelToInt _ = Prelude.error "Label isn't an int"
