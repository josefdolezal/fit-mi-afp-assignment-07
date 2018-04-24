module Data.SortedList where

import Data.List (sort)
import Data.Semigroup

-- | List which is expected to be sorted
data SortedList a = a :<$ SortedList a
                  | Nil
                  deriving (Show, Read, Eq)

-- | Check validity of list (if is sorted)
-- DO NOT CHANGE THIS
valid :: Ord a => SortedList a -> Bool
valid Nil                   = True
valid (_ :<$ Nil)           = True
valid (x :<$ xs@(y :<$ ys)) = x < y && valid xs

-- | Construct sorted list from regular list
-- DO NOT CHANGE THIS
fromList :: Ord a => [a] -> SortedList a
fromList = foldr (:<$) Nil . sort

-- | Get the smallest element in sorted list
-- DO NOT CHANGE THIS
smallest :: Ord a => SortedList a -> a
smallest (x :<$ _) = x
smallest _         = error "SortedList: Nil"

merge :: Ord a => SortedList a -> SortedList a -> SortedList a
merge Nil r                    = r
merge l Nil                    = l
merge l@(x :<$ xs) r@(y :<$ys) = case (x < y) of
    True -> x :<$ (merge xs r)
    _    -> y :<$ (merge l ys)

slconcat :: SortedList a -> SortedList a -> SortedList a
slconcat Nil r        = r
slconcat l Nil        = l
slconcat (l :<$ ls) r = l :<$ (ls `slconcat` r)

instance Ord a => Semigroup (SortedList a) where
    (<>) = merge

instance Ord a => Monoid (SortedList a) where
    mempty = Nil
    mappend = (<>)

instance Functor SortedList where
    fmap _ Nil        = Nil
    fmap f (x :<$ xs) = (f x) :<$ (fmap f xs)

instance Applicative SortedList where
    pure  = (:<$ Nil)
    (<*>) Nil _         = Nil
    (<*>) (f :<$ fs) xs = fmap f xs `slconcat` (fs <*> xs)

instance Monad SortedList where
    (>>=) Nil _        = Nil
    (>>=) (x :<$ xs) f = f x `slconcat` (xs >>= f)
