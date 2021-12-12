module Main where

import Data.String.Strip
import Data.SymList
import Data.List

main :: IO ()
main = do
    let xs = [1]
    let xs' = fromList xs
    let t' = tailSL xs'
    let t = fromSL t'
    let t1 = tail xs
    print $ "tail:" ++ show t
    print $ "tail_1:" ++ show t1
