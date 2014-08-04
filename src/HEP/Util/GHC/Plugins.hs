module HEP.Util.GHC.Plugins where

import Control.Monad.IO.Class
import DynFlags
import GHC 
import GHC.Paths
-- import Unsafe.Coerce
import System.FilePath
--
import HEP.Util.File 


pluginCompile :: FilePath -> String -> String -> IO (Either String HValue)
pluginCompile basedir mname exp =  
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do 
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
          return (Left "Compilation Failed") 
        Succeeded -> do 
          -- m <- findModule (mkModuleName mname) Nothing
          setContext [IIModule (mkModuleName mname)] 
          value <- compileExpr exp 
          return (Right value)          
    return f




