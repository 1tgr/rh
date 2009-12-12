{-# LANGUAGE PatternGuards #-}
module Rh.Source(transformModule, transformFile, transformFileContents) where

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Data
import Data.Generics.PlateData
import qualified Data.Map as Map
import IO
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.ExactPrint
import List

makeSpaces :: Num n => l -> (n, l)
makeSpaces l = (0, l)

applySpaces1M :: (Int, SrcSpanInfo) -> State (Map.Map (Int, Int) Int) (Maybe (Int, SrcSpanInfo))
applySpaces1M (widen, loc) | widen /= 0 = let line = startLine loc
                                              column = startColumn loc
                                           in do get >>= put . Map.insert (line, column) widen
                                                 return Nothing
                           | otherwise = return Nothing

applySpaces2M :: Map.Map (Int, Int) Int -> SrcSpanInfo -> State (Int, Int) SrcSpanInfo
applySpaces2M widenMap loc | SrcSpanInfo { srcInfoSpan = s, srcInfoPoints = p } <- loc
                           = do state <- get
                                let (s', state1) = runState (applySpaces2M' widenMap s) state
                                    (p', state2) = runState (mapM (applySpaces2M' widenMap) p) state
                                put $ max state1 state2
                                return loc { srcInfoSpan = s', srcInfoPoints = p' }
  where applySpaces2M' widenMap srcSpan = do let widen = sum [ a | (pos, a) <- Map.assocs widenMap, within srcSpan pos ]
                                             (line, move) <- resetAtEndOfLine (startLine srcSpan) <$> get
                                             put (line, move + widen)
                                             return srcSpan { srcSpanStartColumn = (srcSpanStartColumn srcSpan) + move,
                                                              srcSpanEndColumn = (srcSpanEndColumn srcSpan) + move + widen }
        within srcSpan pos = let start = (srcSpanStartLine srcSpan, srcSpanStartColumn srcSpan)
                                 end = (srcSpanEndLine srcSpan, srcSpanEndColumn srcSpan)
                              in start <= pos && pos <= end
        resetAtEndOfLine line (lastLine, move) | line == lastLine = (lastLine, move)
                                               | otherwise = (line, 0)

applySpaces :: (Data (f (Int, SrcSpanInfo)), Functor f) => f (Int, SrcSpanInfo) -> f SrcSpanInfo
applySpaces t = let widenMap = flip execState Map.empty $ rewriteBiM applySpaces1M t
                 in fmap snd $ evalState (transformBiM (applySpaces2M widenMap) t) (0, 0)

formatError :: SrcLoc -> String -> String
formatError loc str = "Parse failed at (" ++ show (srcLine loc) ++ ":" ++ show (srcColumn loc) ++ "): " ++ str

transformModule :: (Data (ast (Int, SrcSpanInfo)), Functor ast) => (Module (Int, SrcSpanInfo) -> ast (Int, SrcSpanInfo)) -> Module SrcSpanInfo -> ast SrcSpanInfo
transformModule f = applySpaces . f . fmap makeSpaces

transformFile :: (ExactP ast, Data (ast (Int, SrcSpanInfo)), Functor ast) => (Module (Int, SrcSpanInfo) -> ast (Int, SrcSpanInfo)) -> FilePath -> IO ()
transformFile f path = do putStrLn ("Updating " ++ path)
                          parseFileWithExts glasgowExts path >>= transformFile'
  where transformFile' (ParseOk m) = writeFile path $ flip exactPrint [] $ transformModule f m
        transformFile' (ParseFailed loc str) = ioError $ userError $ formatError loc str

transformFileContents :: (ExactP ast, Data (ast (Int, SrcSpanInfo)), Functor ast) => (Module (Int, SrcSpanInfo) -> ast (Int, SrcSpanInfo)) -> String -> Either String String
transformFileContents f = transformFileContents' . parseFileContentsWithExts glasgowExts
  where transformFileContents' (ParseOk m) = Left $ flip exactPrint [] $ transformModule f m
        transformFileContents' (ParseFailed loc str) = Right $ formatError loc str
