module Main where

import Data.String.Strip
import Data.SymList
import Data.List

main :: IO ()
main = do
    let t = tail ([] :: [Int])
    print $ "tail:" ++ show t
