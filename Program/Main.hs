module Main where

import IO
import Rh.RenameType
import Rh.Source
import System
  
main :: IO Int
main = catch (do oldName : newName : args <- getArgs
                 mapM_ (transformFileOrStdin (renameType oldName newName)) args
                 return 0)
             (\e -> do hPutStrLn stderr (show e)
                       return 1)
