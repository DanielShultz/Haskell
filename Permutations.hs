module Codewars.Kata.Permutations (permutations) where

import Data.List (nub)

permutations :: String -> [String]
permutations xs = nub $ xs : perms xs

perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x:xs) = concatMap (insertElem x) (perms xs) where
      insertElem x [] = [[x]]
      insertElem x yss@(y:ys) = (x:yss) : map (y:) (insertElem x ys)