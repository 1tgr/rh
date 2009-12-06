{-# LANGUAGE PatternGuards #-}
module Main where

import Debug.Trace
import IO
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.ExactPrint
import List
import System

data Tree l = DeclT (Decl l)
            | ExpT (Exp l)
            | ImportDeclT (ImportDecl l)
            | ModuleT (Module l)
            | ModuleHeadT (ModuleHead l)
            | ModuleNameT (ModuleName l)
            | OptionPragmaT (OptionPragma l)
            | XAttrT (XAttr l)
            | XNameT (XName l)

class AnnotatedTree ast where
  tmap :: (Tree l -> Tree l) -> ast l -> ast l

instance AnnotatedTree Module where
  tmap f (Module l mmh ops iss dcls) =
      Module l (fmap ((\(ModuleHeadT   x) -> x) . f . ModuleHeadT  ) mmh) 
               ( map ((\(OptionPragmaT x) -> x) . f . OptionPragmaT) ops) 
               ( map ((\(ImportDeclT   x) -> x) . f . ImportDeclT  ) iss) 
               ( map ((\(DeclT         x) -> x) . f . DeclT        ) dcls)
  tmap f (XmlPage l n ops xn xas me es) =
      XmlPage l (     ((\(ModuleNameT   x) -> x) . f . ModuleNameT  ) n)
                ( map ((\(OptionPragmaT x) -> x) . f . OptionPragmaT) ops) 
                (     ((\(XNameT        x) -> x) . f . XNameT       ) xn) 
                ( map ((\(XAttrT        x) -> x) . f . XAttrT       ) xas) 
                (fmap ((\(ExpT          x) -> x) . f . ExpT         ) me) 
                ( map ((\(ExpT          x) -> x) . f . ExpT         ) es)
  tmap f (XmlHybrid l mmh ops iss dcls xn xas me es) =
      XmlHybrid l (fmap ((\(ModuleHeadT   x) -> x) . f . ModuleHeadT  ) mmh) 
                  ( map ((\(OptionPragmaT x) -> x) . f . OptionPragmaT) ops) 
                  ( map ((\(ImportDeclT   x) -> x) . f . ImportDeclT  ) iss) 
                  ( map ((\(DeclT         x) -> x) . f . DeclT        ) dcls)
                  (     ((\(XNameT        x) -> x) . f . XNameT       ) xn) 
                  ( map ((\(XAttrT        x) -> x) . f . XAttrT       ) xas) 
                  (fmap ((\(ExpT          x) -> x) . f . ExpT         ) me)
                  ( map ((\(ExpT          x) -> x) . f . ExpT         ) es)

transformFile :: (Tree SrcSpanInfo -> Tree SrcSpanInfo) -> FilePath -> IO ()
transformFile f path = do
  putStrLn ("Updating " ++ path)
  result <- parseFileWithExts glasgowExts path
  case result of
    ParseOk m -> writeFile path 
               $ flip exactPrint [] 
               $ tmap f m
    ParseFailed l str -> ioError
                       $ userError 
                       $ "Parse failed at (" ++ show (srcLine l) ++ ":" ++ show (srcColumn l) ++ "): " ++ str

transform :: (SrcInfo l, Show l) => Tree l -> Tree l
transform t | DeclT (DataDecl _ _ _ _ cons _) <- t = trace ("DataDecl: cons = " ++ (concat $ intersperse ", " $ map prettyPrint cons)) t
         | DeclT (PatBind _ pat _ rhs _) <- t = trace ("PatBind: pat = '" ++ prettyPrint pat ++ "', rhs = '" ++ prettyPrint rhs ++ "'") t
         | DeclT (TypeDecl _ _ t') <- t = trace ("TypeDecl: t = " ++ prettyPrint t') t
         | otherwise = t

main :: IO Int
main = catch (do getArgs >>= mapM_ (transformFile transform)
                 return 0)
             (\e -> do hPutStrLn stderr (show e)
                       return 1)