module HEP.Util.Combinatorics where

combinationsOf :: Int -> [a] -> [[a]]
combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf k (x:xs) = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs

combinations :: Int -> Int -> [[Int]]
combinations k n = combinationsOf k [1..n]

combSep :: (Eq a) => Int -> [a] -> [([a],[a])]
combSep k lst = let cmblst = combinationsOf k lst 
                    f x = (x,filter (not.((flip elem) x)) lst)
                in map f cmblst 
