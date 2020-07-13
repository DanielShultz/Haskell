module RangeExtractor.JorgeVS.Kata where

import Data.List

solution :: [Int] -> String
solution a = concat $ intersperse "," (map go (party $ map (:[]) a))
  where party a | (party' a) == a = a
                | otherwise = party $ party' a
        party' [] = []
        party' (x:[]) = x:[]
        party' (x:y:xs)  | (succ $ last x)==(head y) = (x++y) : party' xs
                         | otherwise = x : party' (y:xs)
        go (x:[]) = show x
        go (x:y:[]) = (show x) ++ ',' : (show y)
        go a = (show $ head a) ++ '-' : (show $ last a)