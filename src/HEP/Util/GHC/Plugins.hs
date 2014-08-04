module HEP.Util.GHC.Plugins where

import Control.Monad.IO.Class
import Data.Dynamic
import DynFlags
import GHC 
import GHC.Paths
-- import Unsafe.Coerce
import System.FilePath
--
import HEP.Util.File 


pluginCompile :: Bool -> FilePath -> String -> String -> IO (Either String HValue)
pluginCompile isDynamic basedir mname exp =  
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do 
    let (mdir,mfile) = moduleDirFile mname 
        fp = basedir </> mdir </> mfile
    f <- runGhc (Just libdir) $ do 

      dflags <- getSessionDynFlags
      if isDynamic 
        then setSessionDynFlags (dynamicTooMkDynamicDynFlags dflags)
        else setSessionDynFlags dflags
      target <- guessTarget fp Nothing 

      addTarget target

      r <- load LoadAllTargets 
      case r of 
        Failed -> do 
          return (Left "Compilation Failed") 
        Succeeded -> do 
    
          liftIO $ putStrLn "stage1"
          -- m <- findModule (mkModuleName mname) Nothing
          setContext [IIDecl $ simpleImportDecl (mkModuleName mname)]
             -- [IIModule (mkModuleName mname)] 
          liftIO $ putStrLn "stage2"
          liftIO $ print libdir

          value <- compileExpr exp 
          -- dyn <- dynCompileExpr exp
          -- let value = fromDyn dyn 
          return (Right value)          
    return f




