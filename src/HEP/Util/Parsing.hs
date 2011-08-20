module HEP.Util.Parsing where

import Control.Monad.Identity
import Control.Exception (bracket)

import Text.Parsec

import System.IO

readConfig :: (Show a) => FilePath -> (ParsecT String () Identity a) -> IO a
readConfig fp parser = do 
  putStrLn fp
  bracket (openFile fp ReadMode) hClose $ \fh -> do 
    str <- hGetContents fh 
    let r = parse parser "" str
    case r of 
      Right result -> do putStrLn (show result) 
                         return $! result
      Left err -> error (show err) 
  

