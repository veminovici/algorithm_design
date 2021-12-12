module Data.SymList (
    SymList
  , consSL
  , fromList
  , fromSL
) where

import Data.List

type SymList a = ([a], [a])

-- | Build a SymList from List
fromList :: [a] -> SymList a
fromList = foldr consSL ([], [])

-- | Build a List from a SymList
fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys

-- | Returns True if the list is empty or contains one item.
nullOrSingle :: [a] -> Bool
nullOrSingle [] = True
nullOrSingle [x] = True
nullOrSingle _ = False

-- | Invariants
checkInvariants :: SymList a -> SymList a
checkInvariants (xs, ys)
  | null xs = if nullOrSingle ys then (xs, ys) else error "Invarian 1 broken"
  | null ys = if nullOrSingle xs then (xs, ys) else error "Invariant 2 broken" 
  | otherwise = (xs, ys)

consSL :: a -> SymList a -> SymList a
consSL x ([], ys) = ([x], ys)
consSL x ([y], []) = ([x], [y])
consSL x (xs, ys) = (x : xs, ys)
