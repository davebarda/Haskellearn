module Writer (Writer(..), DiffList(..), tell, toDiffList, fromDiffList, getValFromWriter) where
import Control.Monad (liftM, ap)

newtype Writer w a = Writer { runWriter :: (a, w) }
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

tell :: w -> Writer w ()
tell x = Writer ((), x)

getValFromWriter :: Writer w a -> a
getValFromWriter (Writer (a, _)) = a

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []
