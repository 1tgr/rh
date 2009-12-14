{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
module Rh.RenameType where

import Data.Generics.PlateData
import List
import Rh.Refactoring

renameType :: forall l. (Data l, SrcInfo l) => String -> String -> Module l -> Refactor (Module l)
renameType oldName newName m = rewriteBiM (asstM :: Asst l -> Refactor (Maybe (Asst l))) m >>=
                               rewriteBiM (declHeadM :: DeclHead l -> Refactor (Maybe (DeclHead l))) >>=
                               rewriteBiM (exportSpecM :: ExportSpec l -> Refactor (Maybe (ExportSpec l))) >>=
                               rewriteBiM (instHeadM :: InstHead l -> Refactor (Maybe (InstHead l))) >>=
                               rewriteBiM (typeM :: Type l -> Refactor (Maybe (Type l)))
  where n = (length newName) - (length oldName)
        renamed loc t = do widen n loc
                           return $ Just t
        asstM t       | ClassA  l (UnQual l' (Ident l'' name)) ts  <- t, name == oldName = renamed l'' $ ClassA l (UnQual l' (Ident l'' newName)) ts
                      | otherwise = return Nothing

        declHeadM t   | DHead   l            (Ident l'' name)  tvs <- t, name == oldName = renamed l'' $ DHead l             (Ident l'' newName)  tvs
                      | otherwise = return Nothing

        exportSpecM t | EAbs    l (UnQual l' (Ident l'' name))     <- t, name == oldName = renamed l'' $ EAbs   l (UnQual l' (Ident l'' newName))
                      | otherwise = return Nothing

        instHeadM t   | IHead   l (UnQual l' (Ident l'' name)) ts  <- t, name == oldName = renamed l'' $ IHead l  (UnQual l' (Ident l'' newName)) ts
                      | otherwise = return Nothing

        typeM t       | TyCon   l (UnQual l' (Ident l'' name))     <- t, name == oldName = renamed l'' $ TyCon l  (UnQual l' (Ident l'' newName))
                      | otherwise = return Nothing
