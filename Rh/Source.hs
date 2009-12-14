{-# LANGUAGE PatternGuards #-}
module Rh.Source(transformModule, transformFile, transformFileContents, transformFileOrStdin) where

import Control.Monad.State.Lazy
import Data.Generics.PlateData
import qualified Data.Map as Map
import IO
import Rh.Refactoring

applySpaces :: [ ((Int, Int), Int) ] -> SrcSpanInfo -> SrcSpanInfo
applySpaces assocs loc | SrcSpanInfo { srcInfoSpan = s, srcInfoPoints = p } <- loc
                       = loc { srcInfoSpan = applySpaces' s, srcInfoPoints = map applySpaces' p }
  where applySpaces' srcSpan | SrcSpan { srcSpanStartLine = startLine, srcSpanStartColumn = startColumn,
                                         srcSpanEndLine   = endLine,   srcSpanEndColumn   = endColumn } <- srcSpan
                             = let moveStart = sum [ a | ((line, column), a) <- assocs, line == startLine, column  < startColumn ]
                                   moveEnd   = sum [ a | ((line, column), a) <- assocs, line == endLine  , column <= endColumn ]
                                in srcSpan { srcSpanStartColumn = startColumn + moveStart,
                                             srcSpanEndColumn   = endColumn   + moveEnd }

formatError :: SrcLoc -> String -> String
formatError loc str = "Parse failed at (" ++ show (srcLine loc) ++ ":" ++ show (srcColumn loc) ++ "): " ++ str

transformModule :: (Data (ast SrcSpanInfo), Functor ast) => Refactor (ast SrcSpanInfo) -> ast SrcSpanInfo
transformModule f = let (m, widenMap) = runState f Map.empty
                     in transformBi (applySpaces $ Map.assocs widenMap) m

transformFile :: (ExactP ast, Data (ast SrcSpanInfo), Functor ast) => (Module SrcSpanInfo -> Refactor (ast SrcSpanInfo)) -> FilePath -> IO ()
transformFile f path = do putStrLn ("Updating " ++ path)
                          parseFileWithExts glasgowExts path >>= transformFile'
  where transformFile' (ParseOk m) = writeFile path $ flip exactPrint [] $ transformModule (f m)
        transformFile' (ParseFailed loc str) = ioError $ userError $ formatError loc str

transformFileContents :: (ExactP ast, Data (ast SrcSpanInfo), Functor ast) => (Module SrcSpanInfo -> Refactor (ast SrcSpanInfo)) -> String -> Either String String
transformFileContents f = transformFileContents' . parseFileContentsWithExts glasgowExts
  where transformFileContents' (ParseOk m) = Left $ flip exactPrint [] $ transformModule (f m)
        transformFileContents' (ParseFailed loc str) = Right $ formatError loc str

transformFileOrStdin :: (ExactP ast, Data (ast SrcSpanInfo), Functor ast) => (Module SrcSpanInfo -> Refactor (ast SrcSpanInfo)) -> FilePath -> IO ()
transformFileOrStdin f "-" = getContents >>= transformFileOrStdin' . transformFileContents f
  where transformFileOrStdin' (Left s) = putStr s
        transformFileOrStdin' (Right s) = hPutStrLn stderr s

transformFileOrStdin f path = transformFile f path
