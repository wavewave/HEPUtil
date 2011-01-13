{-# LANGUAGE NoMonomorphismRestriction #-}

module HEP.Util.IDState where

import Control.Monad.State
import Data.Iteratee as Iter
import Data.Iteratee.Util

import LHCOAnalysis

type CounterT = StateT Int 

counteridM :: (Monad m) => a -> CounterT m (Int,a) 
counteridM a = do idee <- get 
                  put $ idee+1
                  return (idee,a)

readjustID :: (Int,PhyEventClassified) -> PhyEventClassified
readjustID (idee,p) = p { eventid = idee } 


combinewithid = jn . (mapStreamM counteridM)

          
 
readjustwithid :: (Monad m) => 
                  Iteratee [PhyEventClassified] (CounterT m) a -> Iteratee [PhyEventClassified] (CounterT m) a
readjustwithid = jn . (mapStreamM (\x -> (return .readjustID) =<< counteridM x ) ) 
                       
--                       ( \x -> do  y <- counteridM x
--                                              return (readjustID y ) ) )    
--                                                   >>= (return . readjustID) )) 