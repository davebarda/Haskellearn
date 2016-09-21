module Writer (Writer(..), DiffList(..), tell, toDiffList, fromDiffList, getValFromWriter) where
import Control.Monad (liftM, ap)

-- Writer data type represents a log writer
newtype Writer w a = Writer { runWriter :: (a, w) }

-- DiffList data type represents a difference list.
-- For more information:
-- https://wiki.haskell.org/Difference_list
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance (Monoid w) => Functor (Writer w) where
  fmap = liftM

instance (Monoid w) =>  Applicative (Writer w) where
  pure  = return
  (<*>) = ap

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')


instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (f.g)


-- A function that is used to create a writer with "dummy" values so a simple text
-- will be writter to the log.
tell :: w -> Writer w ()
tell x = Writer ((), x)

-- A function that is used to get the resulted value from the writer.
getValFromWriter :: Writer w a -> a
getValFromWriter (Writer (a, _)) = a

-- A function that is used to get a difference list from a given standard list
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

-- A function that is used to get a standard list from a given diffrent list.
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []
