module Main where

import Data.String.Strip
import Data.SymList
import Data.List

-- | Returns the online version of the initis
myInits :: [a] -> [[a]]
myInits = map fromSL . scanl (flip snocSL) nilSL

main :: IO ()
main = do
    let xs = [1, 2, 3, 4, 5]
    let ys = myInits xs
    print $ "inits:" ++ show ys
