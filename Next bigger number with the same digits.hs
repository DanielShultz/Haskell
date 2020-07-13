module NextBigger (nextBigger) where

import Data.List

nextBigger :: Int -> Int
nextBigger x = check x $ toArray x
  where toArray 0 = []
        toArray x = (mod x 10) : toArray (div x 10)
        fromArray [] = 0
        fromArray (x:xs) = ((*) x . (^) 10 $ length xs) + (fromArray xs)
        check x xs | (fromArray . reverse $ sort xs) == x = (-1)
                   | otherwise = go x xs
          where  go x xs  | ((toArray $ succ x) \\ xs) == [] = succ x
                          | otherwise = go (succ x) xs