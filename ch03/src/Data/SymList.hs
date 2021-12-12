module Data.SymList (
    SymList
  , consSL
  , fromList
  , fromSL
  , headSL
  , initSL
  , lastSL
  , nullSL
  , snocSL
  , tailSL
) where

import Data.List

type SymList a = ([a], [a])

-- | Build a SymList from List
fromList :: [a] -> SymList a
fromList = foldr consSL ([], [])

-- | Build a List from a SymList
fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys

-- | Returns True if the list is empty or contains one item
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

-- | Returns true if the list is empty
nullSL :: SymList a -> Bool
nullSL ([], []) = True
nullSL _ = False

-- | Inserts the item at the begining of the list
consSL :: a -> SymList a -> SymList a
consSL x ([], ys) = ([x], ys)
consSL x ([y], []) = ([x], [y])
consSL x (xs, ys) = (x : xs, ys)

-- | Inserts the item at the end of the list
snocSL :: a -> SymList a -> SymList a
snocSL x (xs, []) = (xs, [x])
snocSL x ([], [y]) = ([y], [x])
snocSL x (xs, ys) = (xs, x : ys)

-- | Returns the header of the list
headSL :: SymList a -> Maybe a
headSL ([], []) = Nothing
headSL ([], [y]) = Just $ y
headSL ([], _) = error "headSL - invariant2 broken"
headSL (x:_, _) = Just x

-- | Returns the last item ot the list
lastSL :: SymList a -> Maybe a
lastSL ([], []) = Nothing
lastSL ([x], []) = Just x
lastSL (_, []) = error "lastSL - invariant 1 broken"
lastSL (_, y:_) = Just y

-- | Return the tail of the list
tailSL :: SymList a -> SymList a
tailSL ([], []) = ([], [])
tailSL ([], [y]) = ([], [])
tailSL ([], _) = error "tailSL - invariant 2 broken"
tailSL (_:xs, ys) = (xs, ys)

-- | Return the prefix of the list
initSL :: SymList a -> SymList a
initSL ([], []) = ([], [])
initSL ([x], []) = ([], [])
initSL (_, []) = error "initSL - invariant1 broken"
initSL (xs, _:ys) = (xs, ys)
