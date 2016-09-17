module Label(Label(LDouble, LInt), LabelType(DoubleType, IntType), labelToDouble, labelToInt) where

--
data LabelType = DoubleType | IntType deriving Show

--
data Label = LDouble Double | LInt Int deriving Show

--
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

--
labelToDouble :: Label -> Double
labelToDouble (LDouble a) = a
labelToDouble _ = Prelude.error "Label isn't a double"

--
labelToInt :: Label ->  Int
labelToInt (LInt a) = a
labelToInt _ = Prelude.error "Label isn't an int"
