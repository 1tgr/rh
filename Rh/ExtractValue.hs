{-# LANGUAGE PatternGuards #-}
module Rh.ExtractValue(extractValue) where

import Control.Monad.State
import Data.Generics.PlateData
import Debug.Trace
import Rh.Refactoring

dropSrcInfo :: Functor f => f a -> f ()
dropSrcInfo = fmap (\_ -> ())

findExp :: Pos -> Pos -> Module SrcSpanInfo -> Maybe (Exp SrcSpanInfo)
findExp expStart expEnd m | e:_ <- filter (match . ann) (universeBi m) = Just e
                          | otherwise = Nothing
  where match loc = let span = srcInfoSpan loc
                        start = (srcSpanStartLine span, srcSpanStartColumn span)
                        end = (srcSpanEndLine span, srcSpanEndColumn span)
                     in start == expStart && end == expEnd

replace :: Data l => Exp l -> Exp l -> Exp l -> Maybe (Exp l)
replace matchExp withExp inExp = case runState (descendM replaceM inExp) False of
                                   (replacedExp, True) -> Just replacedExp
                                   (_, False) -> Nothing
  where matchExpMinusSrcInfo = dropSrcInfo matchExp
        replaceM e | dropSrcInfo e == matchExpMinusSrcInfo = do put True
                                                                return withExp
                   | otherwise = descendM replaceM e

parseExp :: String -> ParseResult (Exp SrcSpanInfo)
parseExp str = do m <- parseFileContents ("result = " ++ str)
                  case m of
                    Module _ _ _ _ [ PatBind _ _ _ (UnGuardedRhs _ exp) _ ] -> return exp
                    _ -> fail "Failed to parse expression"

extractValue :: Pos -> Pos -> String -> Module SrcSpanInfo -> Refactor (Module SrcSpanInfo)
extractValue expStart expEnd name m = rewriteBiM declM m
  where Just matchExp = findExp expStart expEnd m
        nspan = SrcSpan { srcSpanFilename = "", srcSpanStartLine = 0, srcSpanStartColumn = 0, srcSpanEndLine = 0, srcSpanEndColumn = 0 }
        nloc = SrcSpanInfo { srcInfoSpan = nspan, srcInfoPoints = repeat nspan }
        ParseOk withExp = parseExp name
        declM t | PatBind l p mt (UnGuardedRhs l' r) mb <- t 
                , Just r' <- replace matchExp withExp r
                = let b' = case mb of
                             Just (BDecls l'' binds) -> BDecls l'' binds
                             Just (IPBinds l'' binds) -> IPBinds l'' binds
                             Nothing -> BDecls nloc [ PatBind nloc (PVar nloc (Ident nloc name)) Nothing (UnGuardedRhs nloc withExp) Nothing ]
                      p' = PatBind l p mt (UnGuardedRhs l' r') (Just b')
                   in return $ Just $ trace (show $ dropSrcInfo p') p'
                | otherwise = return Nothing