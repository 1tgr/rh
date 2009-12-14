{-# LANGUAGE PatternGuards #-}
module Rh.ExtractValue(extractValue) where

import Control.Monad.State
import Data.Generics.PlateData
import Debug.Trace
import Rh.Refactoring

dropSrcInfo :: Functor f => f a -> f ()
dropSrcInfo = fmap (\_ -> ())

replaceSrcInfo :: Functor f => a -> f b -> f a
replaceSrcInfo a = fmap (const a)

findExp :: Pos -> Pos -> Module SrcSpanInfo -> Exp SrcSpanInfo
findExp expStart expEnd = head . filter (match . ann) . universeBi
  where match loc = let span = srcInfoSpan loc
                        start = (srcSpanStartLine span, srcSpanStartColumn span)
                        end = (srcSpanEndLine span, srcSpanEndColumn span)
                     in start == expStart && end == expEnd

replace :: Exp () -> Exp () -> Exp () -> Maybe (Exp ())
replace matchExp withExp inExp = case runState (descendM replaceM inExp) False of
                                   (replacedExp, True) -> Just replacedExp
                                   (_, False) -> Nothing
  where replaceM e | e == matchExp = do trace ("\nreplacing \"" ++ (show e) ++ "\" with \"" ++ (show withExp) ++ "\"") $ put True
                                        return withExp
                   | otherwise = descendM replaceM e

extractValue :: Pos -> Pos -> String -> Module SrcSpanInfo -> Refactor (Module SrcSpanInfo)
extractValue expStart expEnd name m = rewriteBiM declM m
  where matchExp = dropSrcInfo $ findExp expStart expEnd m
        nspan = SrcSpan { srcSpanFilename = "", srcSpanStartLine = 0, srcSpanStartColumn = 0, srcSpanEndLine = 0, srcSpanEndColumn = 0 }
        nloc = SrcSpanInfo { srcInfoSpan = nspan, srcInfoPoints = repeat nspan }
        withExp = Var () (UnQual () (Ident () name))
        declM t | PatBind l p mt (UnGuardedRhs l' r) mb <- t 
                , Just r' <- replace matchExp withExp (dropSrcInfo r)
                = let mb' = case mb of
                              --Just (BDecls l'' binds) -> BDecls l'' (??? : binds)
                              --Just (IPBinds l'' binds) -> IPBinds l'' (??? : binds)
                              Nothing -> Just $ BDecls nloc [ PatBind nloc (PVar nloc (Ident nloc name)) Nothing (UnGuardedRhs nloc (replaceSrcInfo nloc withExp)) Nothing ]
                              _ -> undefined
                   in return $ Just $ PatBind l p mt (UnGuardedRhs l' (replaceSrcInfo nloc r')) mb'
                | otherwise = return Nothing