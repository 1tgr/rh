{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
module Rh.RenameType where

import Data.Data
import Data.Generics.PlateData
import Language.Haskell.Exts.Annotated
import List
import Rh.Monads

renameType :: forall l. (Data l, SrcInfo l) => String -> String -> Module l -> Refactor (Module l)
renameType oldName newName m = rewriteBiM (f1 :: Asst l -> Refactor (Maybe (Asst l))) m >>=
                               rewriteBiM (f2 :: DeclHead l -> Refactor (Maybe (DeclHead l))) >>=
                               rewriteBiM (f3 :: InstHead l -> Refactor (Maybe (InstHead l))) >>=
                               rewriteBiM (f4 :: Type l -> Refactor (Maybe (Type l)))
  where w = widen $ (length newName) - (length oldName)
        f1 t | ClassA  l (UnQual l' (Ident l'' name)) ts <- t
             , name == oldName
             = do w l''
                  return $ Just $ ClassA l (UnQual l' (Ident l'' newName)) ts
             | otherwise = return Nothing

        f2 t | DHead   l            (Ident l'' name)  tvs <- t
             , name == oldName
             = do w l''
                  return $ Just $ DHead l             (Ident l'' newName)  tvs
             | otherwise = return Nothing

        f3 t | IHead   l (UnQual l' (Ident l'' name)) ts <- t
             , name == oldName
             = do w l''
                  return $ Just $ IHead l  (UnQual l' (Ident l'' newName)) ts
             | otherwise = return Nothing

        f4 t | TyCon   l (UnQual l' (Ident l'' name)) <- t
             , name == oldName 
             = do w l''
                  return $ Just $ TyCon l  (UnQual l' (Ident l'' newName))
             | otherwise = return Nothing
