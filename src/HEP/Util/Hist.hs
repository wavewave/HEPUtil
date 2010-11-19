{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Analysis.Hist where


import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed


type Hist s = STUArray s Int 
type Hist3D s = STUArray s (Int,Int,Int)


data HistEnv = HistEnv { starting :: Double
                       , ending   :: Double 
                       , step     :: Double 
                       }

build_histogram :: HistEnv -> ST s (Hist s Int)
build_histogram (HistEnv start end stp) 
    = build start end stp
                          
add_to_histogram :: HistEnv -> Hist s Int -> Double -> ST s ()
add_to_histogram histenv hist val =
  let start' = starting histenv
      end'   = ending   histenv
      step'  = step     histenv
      bin'   = findbin start' end' step' val 
  in  if val < start' || val > end'
      then return ()
      else apply hist bin' (+1)


findbin :: Double -> Double -> Double -> Double -> Int 
findbin start _ stp val = let x = fromInteger $ floor $ (val - start) / stp  
                            in{- trace ("start = " ++ show start
                                      ++ ", end = " ++ show end 
                                      ++ ", stp = " ++ show stp
                                      ++ ", val = " ++ show val 
                                      ++ ", x = " ++ show x) -} x 


build :: Double -> Double -> Double -> ST s (Hist s Int) 
build start end stp 
    = do let n = floor ((end - start) / stp )
         hist <- newArray (0,n) 0 
         return hist

peek :: Hist s Int -> Int -> ST s Int
peek hist binnum = readArray hist binnum


poke :: Hist s Int -> Int -> Int -> ST s ()
poke hist binnum val = writeArray hist binnum val

apply :: Hist s Int -> Int -> (Int -> Int) -> ST s ()
apply hist binnum func = do val <- readArray hist binnum
                            writeArray hist binnum (func val)


build3d :: (Int,Int,Int) -> (Int,Int,Int) -> ST s (Hist3D s Int)
build3d (start1,start2,start3) (end1,end2,end3)
    = do hist <- newArray ((start1,start2,start3),(end1,end2,end3)) 0 
         return hist

add_two_hist_array :: UArray Int Int -> UArray Int Int -> UArray Int Int
add_two_hist_array arr1 arr2 
    = let bnds = bounds arr1
          assoc1 = assocs arr1
          assoc2 = assocs arr2
      in accumArray (+) 0 bnds (assoc1 ++ assoc2)
