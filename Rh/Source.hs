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
import Rh.Monads

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
                              in start <= pos && pos < end
        resetAtEndOfLine line (lastLine, move) | line == lastLine = (lastLine, move)
                                               | otherwise = (line, 0)

formatError :: SrcLoc -> String -> String
formatError loc str = "Parse failed at (" ++ show (srcLine loc) ++ ":" ++ show (srcColumn loc) ++ "): " ++ str

transformModule :: Data (ast SrcSpanInfo) => (Module SrcSpanInfo -> Refactor (ast SrcSpanInfo)) -> Module SrcSpanInfo -> ast SrcSpanInfo
transformModule f m = let (m', widenMap) = runState (f m) Map.empty
                       in evalState (transformBiM (applySpaces2M widenMap) m') (0, 0)

transformFile :: (ExactP ast, Data (ast SrcSpanInfo), Functor ast) => (Module SrcSpanInfo -> Refactor (ast SrcSpanInfo)) -> FilePath -> IO ()
transformFile f path = do putStrLn ("Updating " ++ path)
                          parseFileWithExts glasgowExts path >>= transformFile'
  where transformFile' (ParseOk m) = writeFile path $ flip exactPrint [] $ transformModule f m
        transformFile' (ParseFailed loc str) = ioError $ userError $ formatError loc str

transformFileContents :: (ExactP ast, Data (ast SrcSpanInfo), Functor ast) => (Module SrcSpanInfo -> Refactor (ast SrcSpanInfo)) -> String -> Either String String
transformFileContents f = transformFileContents' . parseFileContentsWithExts glasgowExts
  where transformFileContents' (ParseOk m) = Left $ flip exactPrint [] $ transformModule f m
        transformFileContents' (ParseFailed loc str) = Right $ formatError loc str
