{-# LANGUAGE TypeSynonymInstances #-}

-- | 
-- Module      : HEP.Util.Count
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : BSD3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Counter Monad supprot 
-- 

module HEP.Util.Count where

import Control.Monad.IO.Class
import Control.Monad.State

class (Monad m, MonadIO m) => MonadCount m where 
  getCounter :: m Int 
  putCounter :: Int -> m ()

type CountIO = StateT Int IO 

instance MonadCount CountIO where
  getCounter = get 
  putCounter = put
