{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Util.Formatter.Fortran
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- mimicking fortran number formatter 
--
-----------------------------------------------------------------------------

module HEP.Util.Formatter.Fortran where

-- import Data.ByteString.Char8 
import Numeric 


data FFormat a where 
 I :: Int -> FFormat Int
 E :: Int -> Int -> FFormat Double 

data FPair = forall a. P (FFormat a) a 

digitChar :: Int -> Char 
digitChar 0 = '0'
digitChar 1 = '1'
digitChar 2 = '2'
digitChar 3 = '3'
digitChar 4 = '4'
digitChar 5 = '5'
digitChar 6 = '6'
digitChar 7 = '7'
digitChar 8 = '8'
digitChar 9 = '9'
digitChar x = error $ "digitChar: cannot show "++show x++" as a digit character"

eps :: Double 
eps = 1e-90


fformat :: FFormat a -> a -> String
fformat (I n) v = let vstr = show v
                      l = n - length vstr 
                  in if l < 0 then vstr else replicate l ' ' ++ vstr  
fformat (E w d) v = 
  let v' = if v < eps && v > (-eps) then 0 else v 
      (ds,e) = floatToDigits 10 v'
      l = d - length ds
      vstr = map digitChar ds 
      rstr = if l < 0 then take w vstr else vstr ++ replicate l '0' 
      estr = let estr1 = show (abs e)
             in case estr1 of 
                  _x:_y:[] -> estr1
                  x:[] -> '0':x:[]
                  _ -> error "fformat: too large exponent" 
      schr = if e >= 0 then '+' else '-'
      fstr = ('0':'.':rstr) ++ ('E':schr:estr)
      n = w - length fstr
  in if n < 0 then fstr else replicate n ' ' ++ fstr     

fformats :: [FPair] -> String 
fformats = concat . map (\(P f v)->fformat f v)