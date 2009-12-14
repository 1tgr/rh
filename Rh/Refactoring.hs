module Rh.Refactoring(Pos,
                      Refactor, 
                      widen, 
                      Data.Data.Data,
                      module Language.Haskell.Exts.Annotated) where
  
import Control.Monad.State
import Data.Data
import Data.Map
import Language.Haskell.Exts.Annotated

type Pos = (Int, Int)

type Refactor a = State (Map Pos Int) a

widen :: SrcInfo a => Int -> a -> Refactor ()
widen spaces s = get >>= put . insertWith (+) (startLine s, startColumn s) spaces