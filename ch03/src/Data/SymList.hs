module Data.SymList (
    SymList
  , fromLst
  , fromSL
) where

type SymList a = ([a], [a])

-- | Builds a SynLst from a List
fromLst :: [a] -> SymList a
fromLst [] = ([], [])
fromLst (x:xs) = ([x], reverse xs)

-- | Build a List from a SymList
fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys
