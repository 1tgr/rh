{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
module Main where

import Data.Data
import Data.Generics.PlateData
import IO
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.ExactPrint
import List
import System

transformFile :: (Module SrcSpanInfo -> Module SrcSpanInfo) -> FilePath -> IO ()
transformFile f path = do
  putStrLn ("Updating " ++ path)
  result <- parseFileWithExts glasgowExts path
  case result of
    ParseOk m -> writeFile path $ flip exactPrint [] $ f m
    ParseFailed l str -> ioError
                       $ userError
                       $ "Parse failed at (" ++ show (srcLine l) ++ ":" ++ show (srcColumn l) ++ "): " ++ str

renameType :: forall l. Data l => String -> String -> Module l -> Module l
renameType oldName newName = 
  let f1 t | ClassA  l (UnQual l' (Ident l'' name)) ts <- t,  name == oldName = Just $ ClassA l (UnQual l' (Ident l'' newName)) ts
           | otherwise = Nothing

      f2 t | DHead   l            (Ident l'  name)  tvs <- t, name == oldName = Just $ DHead l             (Ident l'  newName)  tvs
           | otherwise = Nothing

      f3 t | IHead   l (UnQual l' (Ident l'' name)) ts <- t,  name == oldName = Just $ IHead l  (UnQual l' (Ident l'' newName)) ts
           | otherwise = Nothing

      f4 t | TyCon   l (UnQual l' (Ident l'' name)) <- t,     name == oldName = Just $ TyCon l  (UnQual l' (Ident l'' newName))
           | otherwise = Nothing
  
  in rewriteBi (f1 :: Asst l -> Maybe (Asst l))
   . rewriteBi (f2 :: DeclHead l -> Maybe (DeclHead l))
   . rewriteBi (f3 :: InstHead l -> Maybe (InstHead l))
   . rewriteBi (f4 :: Type l -> Maybe (Type l))

main :: IO Int
main = catch (do oldName : newName : args <- getArgs
                 mapM_ (transformFile (renameType oldName newName)) args
                 putStrLn "Finished"
                 return 0)
             (\e -> do hPutStrLn stderr (show e)
                       return 1)
