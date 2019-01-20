module Frontend.Datatypes where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map
import Data.Set
import Data.Void

import Frontend.AbsLatte


type SPos = Maybe (Int, Int)
type TType = Type SPos
type TArg = Arg SPos
type TStmt = Stmt SPos
type TExpr = Expr SPos
type TProgram = Program SPos
type TTopDef = TopDef SPos
type TRelOp = RelOp SPos

data ComputationStatus = Running | MaybeEnded | Ended
type Loc = Int
data Env = Env {
  vars :: Data.Map.Map Ident Loc,
  usedNames :: Data.Set.Set Ident,
  computationStatus :: ComputationStatus
}
type Store = Data.Map.Map Loc TType

type TypeCheckResult = ExceptT String IO
type PartialResult a = StateT Store (ReaderT Env TypeCheckResult) a
