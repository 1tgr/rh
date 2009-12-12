{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}
module Rh.RenameType where

import Data.Data
import Data.Generics.PlateData
import Language.Haskell.Exts.Annotated
import List

renameType :: forall l. Data l => String -> String -> Module (Int, l) -> Module (Int, l)
renameType oldName newName = rewriteBi (f1 :: Asst (Int, l) -> Maybe (Asst (Int, l)))
                           . rewriteBi (f2 :: DeclHead (Int, l) -> Maybe (DeclHead (Int, l)))
                           . rewriteBi (f3 :: InstHead (Int, l) -> Maybe (InstHead (Int, l)))
                           . rewriteBi (f4 :: Type (Int, l) -> Maybe (Type (Int, l)))
  where widen = (length newName) - (length oldName)
        f1 t | ClassA  l (UnQual l' (Ident (n, l'') name)) ts <- t,  name == oldName = Just $ ClassA l (UnQual l' (Ident (n + widen, l'') newName)) ts
             | otherwise = Nothing

        f2 t | DHead   l            (Ident (n, l'') name)  tvs <- t, name == oldName = Just $ DHead l             (Ident (n + widen, l'') newName)  tvs
             | otherwise = Nothing

        f3 t | IHead   l (UnQual l' (Ident (n, l'') name)) ts <- t,  name == oldName = Just $ IHead l  (UnQual l' (Ident (n + widen, l'') newName)) ts
             | otherwise = Nothing

        f4 t | TyCon   l (UnQual l' (Ident (n, l'') name)) <- t,     name == oldName = Just $ TyCon l  (UnQual l' (Ident (n + widen, l'') newName))
             | otherwise = Nothing
