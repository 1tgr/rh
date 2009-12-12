module Main where

import IO
import Rh.RenameType
import Rh.Source
import System
  
main :: IO Int
main = catch (do oldName : newName : args <- getArgs
                 mapM_ (transformFile (renameType oldName newName)) args
                 putStrLn "Finished"
                 return 0)
             (\e -> do hPutStrLn stderr (show e)
                       return 1)
