module Calculator where

evaluate :: String -> Double
evaluate = read . head . pr2 . pr1 . up . words

up :: [String] -> [String]
up [] = []
up ("(":xs) = up (down xs)
up (x:xs) = x : up xs

down :: [String] -> [String]
down [] = []
down (x:")":xs) = x:xs
down a = down . pr1 $ pr2 a

pr1 :: [String] -> [String]        
pr1 [] = []
pr1 (")":xs) = ")" : xs
pr1 (x:[]) = x:[]
pr1 (x:"*":y:xs) = pr1 ((show $ (*) (read x :: Double) (read y :: Double)) : xs)
pr1 (x:"/":y:xs) = pr1 ((show $ (/) (read x :: Double) (read y :: Double)) : xs)
pr1 (x:xs) = x : pr1 xs
        
pr2 :: [String] -> [String]        
pr2 [] = []
pr2 (")":xs) = ")" : xs
pr2 (x:[]) = x:[]
pr2 (x:"-":y:xs) = pr2 ((show $ (-) (read x :: Double) (read y :: Double)) : xs)
pr2 (x:"+":y:xs) = pr2 ((show $ (+) (read x :: Double) (read y :: Double)) : xs)
pr2 (x:xs) = x : pr2 xs