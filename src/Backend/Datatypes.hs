module Backend.Datatypes where

import Control.Monad.State
import qualified Data.Set
import qualified Data.Map

import Frontend.AbsLatte


data StateLLVM = StateLLVM {
  nextRegister :: Integer,
  fnRtypes :: Data.Map.Map String TType,
  globalVarsDefs :: [String]
} deriving (Show)

initialState = StateLLVM 1 Data.Map.empty []

type Result a = StateT StateLLVM IO a
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