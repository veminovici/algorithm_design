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

fromRAList :: RAList a -> [a]
fromRAList = concatMap from 
    where 
    from Zero = []
    from (One t) = fromTree t
