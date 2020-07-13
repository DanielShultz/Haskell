module Snail where

snail :: [[Int]] -> [Int]
snail = cycle 0
  where cycle _ [] = []
        cycle x a | x == 0 = (head a) ++ (cycle 1 (tail a))
                  | x == 1 = (map last a) ++ (cycle 2 (map init a))
                  | x == 2 = (reverse $ last a) ++ (cycle 3 (init a))
                  | x == 3 = (reverse $ map head a) ++ (cycle 0 (map tail a))