{-# LANGUAGE NoMonomorphismRestriction #-}

module HEP.Util.IDState where

import Control.Monad.State
import Data.Iteratee.Util

type CounterT = StateT Int 

counteridM :: (Monad m) => a -> CounterT m (Int,a) 
counteridM a = do idee <- get 
                  put $ idee+1
                  return (idee,a)

combinewithid = jn . (mapStreamM counteridM)
