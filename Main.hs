{-# LANGUAGE PatternGuards #-}
module Main where

import Data.Data
import Debug.Trace
import IO
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.ExactPrint
import List
import Rh.Syntax
import System

transformFile :: (Tree SrcSpanInfo -> Tree SrcSpanInfo) -> FilePath -> IO ()
transformFile f path = do
  putStrLn ("Updating " ++ path)
  result <- parseFileWithExts glasgowExts path
  case result of
    ParseOk m -> writeFile path $ flip exactPrint [] $ (untree (f (tree m)) :: Module SrcSpanInfo)
    ParseFailed l str -> ioError
                       $ userError
                       $ "Parse failed at (" ++ show (srcLine l) ++ ":" ++ show (srcColumn l) ++ "): " ++ str

renameType :: (Data l, SrcInfo l) => String -> String -> Tree l -> Tree l
renameType oldName newName = f
  where f t | AsstT     (ClassA  l (UnQual l' (Ident l'' name)) ts)  <- t, name == oldName = tree $ ClassA l (UnQual l' (Ident l'' newName)) (map (tmap f) ts)
            | DeclHeadT (DHead   l            (Ident l'  name)  tvs) <- t, name == oldName = tree $ DHead l             (Ident l'  newName)  (map (tmap f) tvs)
            | InstHeadT (IHead   l (UnQual l' (Ident l'' name)) ts)  <- t, name == oldName = tree $ IHead l  (UnQual l' (Ident l'' newName)) (map (tmap f) ts)
            | TypeT     (TyCon   l (UnQual l' (Ident l'' name)))     <- t, name == oldName = tree $ TyCon l  (UnQual l' (Ident l'' newName))
            | otherwise = tmap2 f t

main :: IO Int
main = catch (do oldName : newName : args <- getArgs
                 mapM_ (transformFile (renameType oldName newName)) args
                 putStrLn "Finished"
                 return 0)
             (\e -> do hPutStrLn stderr (show e)
                       return 1)
