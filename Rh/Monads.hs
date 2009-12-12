module Rh.Monads(Refactor, widen) where
  
import Control.Monad.State
import Data.Map
import Language.Haskell.Exts.Annotated

type Refactor a = State (Map (Int, Int) Int) a

widen :: SrcInfo a => Int -> a -> Refactor ()
widen spaces s = get >>= put . insert (startLine s, startColumn s) spaces