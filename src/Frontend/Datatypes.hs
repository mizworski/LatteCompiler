module Frontend.Datatypes where

import Data.Map
import Data.Set
import Data.Void
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Frontend.AbsLatte

type SPos = Maybe (Int, Int)
type TType = Type SPos
type Computation = (EndStatus, Env)
data EndStatus = Running | MaybeEnded | Ended


type TArg = Arg (Maybe (Int, Int))
type TStmt = Stmt (Maybe (Int, Int))
type TExpr = Expr (Maybe (Int, Int))
type TProgram = Program (Maybe (Int, Int))
type TTopDef = TopDef (Maybe (Int, Int))

type Loc = Int
data Env = Env {
  vars :: Data.Map.Map Ident Loc,
  usedNames :: Data.Set.Set Ident
}

type Store = Data.Map.Map Loc TType

type TypeCheckResult = ExceptT String IO
type PartialResult a = StateT Store (ReaderT Env TypeCheckResult) a
