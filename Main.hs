{-# LANGUAGE PatternGuards #-}
module Main where

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

renameType :: String -> String -> Module SrcSpanInfo -> Module SrcSpanInfo
renameType oldName newName = 
  let f1 :: Asst SrcSpanInfo -> Maybe (Asst SrcSpanInfo)
      f2 :: DeclHead SrcSpanInfo -> Maybe (DeclHead SrcSpanInfo)
      f3 :: InstHead SrcSpanInfo -> Maybe (InstHead SrcSpanInfo)
      f4 :: Type SrcSpanInfo -> Maybe (Type SrcSpanInfo)
  
      f1 t | ClassA  l (UnQual l' (Ident l'' name)) ts <- t,  name == oldName = Just $ ClassA l (UnQual l' (Ident l'' newName)) ts
           | otherwise = Nothing

      f2 t | DHead   l            (Ident l'  name)  tvs <- t, name == oldName = Just $ DHead l             (Ident l'  newName)  tvs
           | otherwise = Nothing

      f3 t | IHead   l (UnQual l' (Ident l'' name)) ts <- t,  name == oldName = Just $ IHead l  (UnQual l' (Ident l'' newName)) ts
           | otherwise = Nothing

      f4 t | TyCon   l (UnQual l' (Ident l'' name)) <- t,     name == oldName = Just $ TyCon l  (UnQual l' (Ident l'' newName))
           | otherwise = Nothing
  
  in rewriteBi f1
   . rewriteBi f2 
   . rewriteBi f3 
   . rewriteBi f4

main :: IO Int
main = catch (do oldName : newName : args <- getArgs
                 mapM_ (transformFile (renameType oldName newName)) args
                 putStrLn "Finished"
                 return 0)
             (\e -> do hPutStrLn stderr (show e)
                       return 1)
