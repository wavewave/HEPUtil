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

import Control.Applicative ((<$>))
import Numeric 


data FFormat a where 
 I :: Int -> FFormat Int
 E :: Int -> Int -> FFormat Double 
 F :: Int -> Int -> FFormat Double 

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

zerocheck :: Double -> Double 
zerocheck v = if v < eps && v > (-eps) then 0 else v

fillWidthWithSpace :: Int -> String -> String 
fillWidthWithSpace w str = let n = w - length str 
                           in if n < 0 then take w str else replicate n ' ' ++ str

roundDigitsAt :: Int -> [Int] -> Either () [Int] 
roundDigitsAt n xs 
  | length xs < n = error "roundDigitsAt: n must be larger than number of digits"
  | otherwise = let (h,t) = splitAt n xs 
                in case t of 
                     [] -> return h 
                     x:_ -> if x < 5 
                             then return h 
                             else reverse <$> (spillOver . reverse) h

spillOver :: [Int] -> Either () [Int] 
spillOver [] = Left ()
spillOver (x:xs)
  | x < 9 = return ((x+1):xs)
  | otherwise = (0:) <$> spillOver xs 
                   


-- | fortran format 
fformat :: FFormat a -> a -> String
fformat (I n) v = let vstr = show v
                      l = n - length vstr 
                  in if l < 0 then vstr else replicate l ' ' ++ vstr  
fformat (E w d) v =
  let v' = zerocheck v 
      (ds,e) = floatToDigits 10 (abs v')
      l = d - length ds
      vstr = map digitChar ds 
      (rstr,e') = either (const ('1':replicate (d-1) '0',e+1)) (\r->(r,e)) $ 
                    if l < 0 
                    then map digitChar <$> roundDigitsAt d ds 
                    else return (vstr ++ replicate l '0')
      estr = let estr1 = show (abs e')
             in case estr1 of 
                  _x:_y:[] -> estr1
                  x:[] -> '0':x:[]
                  _ -> error "fformat: too large exponent" 
      vschrf = if v' >= 0 then id else (:) '-' 
      eschr = if e >= 0 then '+' else '-'
      fstr = vschrf $ ('0':'.':rstr) ++ ('E':eschr:estr) 
  in (fillWidthWithSpace w fstr)
fformat (F w d) v = 
  let v' = zerocheck v 
      (ds,e) = floatToDigits 10 (abs v') 
      l = d  - (length ds - e) 
      (ds',e') = either (const (1:replicate e 0,e+1)) (\r->(r,e)) $ 
                    roundDigitsAt (e+d) ds 
      vstr = map digitChar ds' 
      hstr = if e' > 0 then take e' vstr else "0"
      lstr1 = drop e' vstr 
      lstr = if l < 0 then take d lstr1 else lstr1
      schrf = if v >= 0 then id else (:) '-' 
      fstr = schrf $ hstr ++ "." ++ lstr 
  in fillWidthWithSpace w fstr 


-- | fortran format in a single list for convenience 
fformats :: [FPair] -> String 
fformats = concat . map (\(P f v)->fformat f v)