module Lib
    ( someFunc
    ) where
import GHC.Float (int2Double)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

vxMaximum :: Ord a => [a] -> Maybe a
vxMaximum [] = Nothing 
vxMaximum [x] = Just x
vxMaximum (x:xs) = (max <$> Just x) <*> vxMaximum xs

vxTake :: Int -> [a] -> [a]
vxTake 0 xs = []
vxTake n [] = []
vxTake n (x:xs) = x : vxTake (n - 1) xs

vxTakeWhile :: (a -> Bool) -> [a] -> [a]
vxTakeWhile p [] = []
vxTakeWhile p (x:xs) | p x = x : vxTakeWhile p xs
vxTakeWhile p (_:xs) = []

vxDrop :: Int -> [a] -> [a]
vxDrop 0 xs = xs
vxDrop n [] = []
vxDrop n (x:xs) = vxDrop (n - 1) xs

vxUncons :: [a] -> Maybe (a, [a])
vxUncons [] = Nothing
vxUncons (x:xs) = Just (x, xs)

vxWrap :: a -> [a]
vxWrap x = [x]

single :: [a] -> Bool
single [x] = True 
single _ = False

vxReverse :: [a] -> [a]
vxReverse = foldl (flip (:)) []

vxMap :: (a -> b) -> [a] -> [b]
vxMap f = foldr op [] where op x xs = f x : xs

vxFilter :: (a -> Bool) -> [a] -> [a]
vxFilter f = foldr op [] where op x xs = [x | f x] ++ xs

vxDropWhileEnd :: (a -> Bool) -> [a] -> [a]
vxDropWhileEnd f xs = snd $ foldr (\x (p, acc) -> if p && f x then (p, acc) else (False, x : acc)) (True, []) xs

vxInteger :: [Int] -> Int
vxInteger = foldl (\acc x -> 10 * acc + x) 0

vxFraction :: [Int] -> Double
vxFraction = foldr (\x acc -> int2Double x + acc * 0.1) 0.0
