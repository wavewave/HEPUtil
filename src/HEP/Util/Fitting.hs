{-# OPTIONS_GHC -fglasgow-exts #-}

module LHCOAnalysis.Analysis.Fitting where

import Debug.Trace

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
 
--import LHCOAnalysis.ROOTApp
import HROOT

import Foreign.ForeignPtr


makeCfunction :: (Double -> Double) -> CFunction
makeCfunction f = \x voidpointer -> realToFrac $ f (realToFrac x)

foreign import ccall "wrapper"
  makeFunPtr :: CFunction -> IO (FunPtr CFunction)
                
                
makeCfunctionPtr :: (Double -> Double -> Double -> Double -> Double)
                  -> CFunctionPtr

makeCfunctionPtr f = func 
  where func xarg pararg voidpointer =  
          do (x:_)        <- peekArray 1 xarg
             (a1:a2:a3:_) <- peekArray 3 pararg
             let result = realToFrac $ f (realToFrac a1) 
                                         (realToFrac a2)
                                         (realToFrac a3)
                                         (realToFrac x)
             return result
   
foreign import ccall "wrapper"
  makeFunPtrPtr :: (CFunctionPtr) -> IO (FunPtr CFunctionPtr) 

fitting ::  TH1F -> Double -> Double -> [Double] -> 
            (Double -> Double -> Double -> Double -> Double ) -> IO ()
fitting hist@(TH1F fptr) start end param func = 
  do let rptr = unsafeForeignPtrToPtr fptr
         c_start = realToFrac start 
         c_end   = realToFrac end
         
         paramtemp = map realToFrac param
     c_param <- newArray paramtemp
     let c_func = makeCfunctionPtr func
     c_funcptr <- makeFunPtrPtr c_func
     
     c_fit_histogram rptr c_start c_end c_param c_funcptr
     return ()

{--trace ("TEST = " ++ show overall ++ ": xx = " ++ show xx ++ ": xmid = " ++ show xmid ++ ": xmax = " ++ show xmax ) $ --}

fitinvm norm eta overall x =  overall / sinh (2.0*eta) * piece xx
  where xx = x / norm
        xmid = sqrt (2.0*(cosh (2.0*eta)-sinh (2.0*eta)))
        xmax = sqrt (2.0*(cosh (2.0*eta)+sinh (2.0*eta))) 
        piece xx | xx <= 0               
             = 0 
        piece xx | xx > 0 && xx <= xmid  
             = 2.0*xx*log (xmax/xmid) 
        piece xx | xx > xmid && xx < xmax
             = 2.0*xx*log (xmax/xx)
        piece xx | xx >= xmax 
             = 0.0
        


