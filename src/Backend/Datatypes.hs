module Backend.Datatypes where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Set
import qualified Data.Map

import Frontend.AbsLatte


type Loc = Int
type Env = Data.Map.Map Ident Loc
data StateLLVM = StateLLVM {
  nextRegister :: Integer,
  globalVarsDefs :: [String],
  varsStore :: Data.Map.Map Loc (TType, Register)
} deriving (Show)

initialState = StateLLVM 1 [] Data.Map.empty

type Result a = StateT StateLLVM (ReaderT Env (ExceptT String IO)) a
data TStatus = Running | Terminated
  deriving (Eq, Ord, Show, Read)

type Instructions = [String]
type Register = String

type SPos = Maybe (Int, Int)
type TType = Type SPos
type TArg = Arg SPos
type TStmt = Stmt SPos
type TExpr = Expr SPos
type TProgram = Program SPos
type TTopDef = TopDef SPos
type TBlock = Block SPos