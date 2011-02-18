module HEP.Util.Printf where

import Text.Printf

import Numeric.LinearAlgebra

showmat :: Matrix Double -> String 
showmat mat = concat [ rowprint i ++ "\n" | i <- [0..(r-1)] ]
  where r = rows mat
        c = cols mat 
        rowprint r = concat [ printf "%1.2e " (mat @@> (r,i)) | i <- [0..(c-1)] ] 
