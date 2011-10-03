module HEP.Util.GHC.Plugins where

import Control.Monad.IO.Class
import GHC 
import GHC.Paths
import DynFlags
-- import Unsafe.Coerce

-- import Control.Monad
import System.FilePath

-- import Data.List 
-- import Data.List.Split

import HEP.Util.File 


pluginCompile :: FilePath -> String -> String -> IO (Either String HValue)
pluginCompile basedir mname exp =  
  defaultErrorHandler defaultDynFlags $ do 
    let (mdir,mfile) = moduleDirFile mname 
        fp = basedir </> mdir </> mfile
    f <- runGhc (Just libdir) $ do 
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      target <- guessTarget fp Nothing 
      addTarget target
      r <- load LoadAllTargets 
      case r of 
        Failed -> do 
          -- putStrLn "Compilation Failed"
          return (Left "Compilation Failed") 
        Succeeded -> do 
          -- putStrLn "Compilation Successed"
          m <- findModule (mkModuleName mname) Nothing
          setContext [] [(m,Nothing)]
          value <- compileExpr exp 
          return (Right value)          
    return f




