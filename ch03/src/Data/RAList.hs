module Data.RAList (
    Digit (..)
  , RAList
  , Tree
  , leaf
  , node
  , size
) where

data Tree a
    = Leaf a
    | Node Int (Tree a) (Tree a)

data Digit a
    = Zero
    | One (Tree a)

type RAList a = [Digit a]

-- | Returns the size of a tree
size :: Tree a -> Int
size (Leaf _) = 1
size (Node n _ _) = n

-- | Builds a leaf
leaf = Leaf

-- | Builds a node
node :: Tree a -> Tree a -> Tree a
node t1 t2 = Node (size t1 + size t2) t1 t2

-- | Returns the list of items stored into a given tree.
fromTree :: Tree a -> [a]
fromTree (Leaf x) = [x]
fromTree (Node _ t1 t2) = fromTree t1 ++ fromTree t2

-- | Returns the collection of items
fromRAList :: RAList a -> [a]
fromRAList = concatMap from 
    where 
    from Zero = []
    from (One t) = fromTree t

-- | Returns true if the list is empty
nullRA :: RAList a -> Bool
nullRA [] = True
nullRA [Zero] = True
nullRA _ = False

-- | Returns the empty list
nilRA = []

-- | Adds a new element at the begining of the list
consRA :: a -> RAList a -> RAList a
consRA = undefined

-- | Splits the list in a pair of header and tail
unconsRA :: RAList a -> (a, RAList a)
unconsRA = undefined 

-- | Updates the item at the given position
updateRA :: Int -> a -> RAList a -> RAList a
updateRA = undefined

fetchTree :: Int -> Tree a -> a
fetchTree 0 (Leaf x) = x
fetchTree k (Node n t1 t2) = 
    if k < m
    then fetchTree k t1 else fetchTree (k - m) t2
    where
    m = n `div` 2
fetchTree _ _ = error "fetchTree - unexpected"

fetchRA :: Int -> RAList a -> a
fetchRA k (Zero:xs) = fetchRA k xs
fetchRA k (One t:xs) = let s = size t in if k < s then fetchTree k t else fetchRA (k - s) xs
fetchRA _ _ = error "fetchRA - unexpected"
