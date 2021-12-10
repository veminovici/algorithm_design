module Main where

import Lib

--
-- INDUCTIVE
--

-- The idiom foldr (concatMap · steps) e will be used frequently

perms1 :: [a] -> [[a]]
perms1 [] = [[]]
perms1 (x:xs) = [ zs | ys <- perms1 xs, zs <- inserts x ys]

inserts :: t -> [t] -> [[t]]
inserts x [] = [[x]]
inserts x (y:ys) = (x:y:ys) : map (y:) (inserts x ys)

perms2 :: [a] -> [[a]]
perms2 = foldr step [[]]
    where
        step x xss = concatMap (inserts x) xss

--
-- RECURSIVE
--

perms3 :: [a] -> [[a]]
perms3 [] =[[]]
perms3 xs = [x:zs| (x,ys) <- picks xs, zs <- perms3 ys]

-- picks an arbitrary element from a list in all possible ways, returning both the element and what remains
picks :: [a] -> [(a, [a])]
picks [] = []
picks (x:xs) = (x, xs) : [(y, x:ys)| (y, ys) <- picks xs]

perms4 :: [a] -> [[a]]
perms4 [] = [[]]
perms4 xs = concatMap subperms (picks xs)
    where
            subperms (x, ys) = map (x:) (perms4 ys)

main :: IO ()
main = do
    let xs = inserts 1 [2, 3]
    print $ "xs=" ++ show xs

    let xs = perms1 [1, 2, 3]
    print $ "xs=" ++ show xs

    let xs = perms2 [1, 2, 3]
    print $ "xs=" ++ show xs
