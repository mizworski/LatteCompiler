module Backend.Datatypes where

import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map

import Frontend.AbsLatte


data StateLLVM = StateLLVM {
  nextRegister :: Integer,
  namespace :: Set.Set Ident
} deriving (Show)

initialState = StateLLVM 1 Set.empty

type Result state a = StateT state IO a

type SPos = Maybe (Int, Int)
type TType = Type SPos
type TArg = Arg SPos
type TStmt = Stmt SPos
type TExpr = Expr SPos
type TProgram = Program SPos
type TTopDef = TopDef SPos