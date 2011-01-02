module HEP.Util.Combinatorics where

import Numeric.LinearAlgebra

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

permutations :: [a] -> [[a]] 
permutations = undefined



shufflematrix :: (Int,Int) -> ((Int->Int),(Int->Int)) -> Matrix Double -> Matrix Double
shufflematrix (n1,n2) (perm1,perm2) m1 = 
  (n1><n2) [ m1 @@> (perm1 i,perm2 j)  | i <- [0..(n1-1)], j <- [0..(n2-1)] ]  
