module Frontend.Datatypes where

import Data.Map
import Data.Void
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Frontend.AbsLatte

type TType = Type (Maybe (Int, Int))
type TArg = Arg (Maybe (Int, Int))
type TProgram = Program (Maybe (Int, Int))
type TTopDef = TopDef (Maybe (Int, Int))

type Loc = Int
type Env = Data.Map.Map Ident Loc
type Store = Data.Map.Map Loc TType

type TypeCheckResult = ExceptT String IO
type PartialResult a = StateT Store (ReaderT Env TypeCheckResult) a
