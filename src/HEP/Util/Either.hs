-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Util.Either
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- utility for either and maybe 
-- 
-----------------------------------------------------------------------------

module HEP.Util.Either where

import Control.Applicative ((<$>))
import Control.Monad 
import Control.Monad.Trans
import Control.Monad.Trans.Either 
import Data.Maybe 

guardEither :: (Monad m) => String -> Bool -> EitherT String m () 
guardEither str b = when b $ EitherT (return (Left str))
                    
guardEitherM :: (Monad m) => String -> m Bool -> EitherT String m () 
guardEitherM str act = lift act >>= guardEither str    

maybeToEither :: e -> Maybe a -> Either e a 
maybeToEither e = maybe (Left e) Right 

boolToMaybe :: (Functor m, Monad m) => Bool -> m a -> m (Maybe a) 
boolToMaybe b act = if b then Just <$> act else return Nothing

boolToMaybeM :: (Functor m, Monad m) => m Bool -> m a -> m (Maybe a) 
boolToMaybeM actb act = actb >>= \b -> boolToMaybe b act 
