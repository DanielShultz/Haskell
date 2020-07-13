module FormatDuration where

formatDuration :: (Integral i, Show i) => i -> String
formatDuration n | n==0 = "now"
                 | otherwise = go $ check ((time (div n 31536000) 1000 "year")  : (time (div n 86400) 365 "day") : (time (div n 3600) 24 "hour") : (time (div n 60) 60 "minute") : (time n 60 "second") : [])
 where time n t str  | (mod n t) == 1 = "1 " ++ str
                     | (mod n t) > 0 = (show $ mod n t) ++ ' ' : str ++ "s"
                     | otherwise = ""

       check [] = []
       check ("":xs) = check xs
       check (x:xs) = x : check xs

       go [] = []
       go (x:[]) = x
       go (x:y:[]) = x ++ " and " ++ y
       go (x:xs) = x ++ ", " ++ go xs