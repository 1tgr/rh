{-# LANGUAGE PatternGuards #-}
module Rh.Source(transformModule, transformFile, transformFileContents) where

import Control.Monad.State.Lazy
import Data.Data
import Data.Generics.PlateData
import qualified Data.Map as Map
--import Debug.Trace
import IO
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.ExactPrint
import Rh.Monads

trace :: String -> a -> a
trace _ = id

showSpan :: SrcSpan -> String
showSpan srcSpan = let startLine = srcSpanStartLine srcSpan
                       startColumn = srcSpanStartColumn srcSpan
                       endLine = srcSpanEndLine srcSpan
                       endColumn = srcSpanEndColumn srcSpan
                    in show startLine ++ ":" ++ show startColumn ++ ".." ++ show endLine ++ ":" ++ show endColumn

applySpaces :: Map.Map (Int, Int) Int -> SrcSpanInfo -> SrcSpanInfo
applySpaces widenMap loc | SrcSpanInfo { srcInfoSpan = s, srcInfoPoints = p } <- loc
                         = loc { srcInfoSpan = applySpaces' s, srcInfoPoints = map applySpaces' p }
  where assocs = Map.assocs widenMap
        applySpaces' srcSpan = let startLine = srcSpanStartLine srcSpan
                                   startColumn = srcSpanStartColumn srcSpan
                                   endLine = srcSpanEndLine srcSpan
                                   endColumn = srcSpanEndColumn srcSpan
                                   moveStart = sum [ a | ((line, column), a) <- assocs, line == startLine, column < startColumn ]
                                   moveEnd   = sum [ a | ((line, column), a) <- assocs, line == endLine, column <= endColumn ]
                                in case (moveStart, moveEnd) of
                                    (0, 0) -> srcSpan
                                    _ -> trace (showSpan srcSpan ++ " moves by " ++ show (moveStart, moveEnd))
                                          $ srcSpan { srcSpanStartColumn = startColumn + moveStart,
                                                      srcSpanEndColumn   = endColumn   + moveEnd }

formatError :: SrcLoc -> String -> String
formatError loc str = "Parse failed at (" ++ show (srcLine loc) ++ ":" ++ show (srcColumn loc) ++ "): " ++ str

transformModule :: (Data (ast SrcSpanInfo), Functor ast, Show (ast String)) => Refactor (ast SrcSpanInfo) -> ast SrcSpanInfo
transformModule f = let (m, widenMap) = runState f Map.empty
                        message = "\nModule after =  " ++ (show $ fmap (showSpan . srcInfoSpan) m) ++ "\nMap = " ++ (show widenMap)
                     in transformBi (applySpaces widenMap) $ trace message m

transformFile :: (ExactP ast, Data (ast SrcSpanInfo), Functor ast, Show (ast String)) => (Module SrcSpanInfo -> Refactor (ast SrcSpanInfo)) -> FilePath -> IO ()
transformFile f path = do putStrLn ("Updating " ++ path)
                          parseFileWithExts glasgowExts path >>= transformFile'
  where transformFile' (ParseOk m) = writeFile path $ flip exactPrint [] $ transformModule (f m)
        transformFile' (ParseFailed loc str) = ioError $ userError $ formatError loc str

transformFileContents :: (ExactP ast, Data (ast SrcSpanInfo), Functor ast, Show (ast String)) => (Module SrcSpanInfo -> Refactor (ast SrcSpanInfo)) -> String -> Either String String
transformFileContents f = transformFileContents' . parseFileContentsWithExts glasgowExts
  where transformFileContents' (ParseOk m) = Left $ flip exactPrint [] $ transformModule $ trace ("\nModule before = " ++ (show $ fmap (showSpan . srcInfoSpan) m)) (f m)
        transformFileContents' (ParseFailed loc str) = Right $ formatError loc str
