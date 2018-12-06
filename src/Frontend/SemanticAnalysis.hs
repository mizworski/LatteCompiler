module Frontend.SemanticAnalysis where

import Frontend.AbsLatte
import Frontend.Datatypes
import Data.Map
import qualified Data.Set
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except


semanticAnalysis :: TProgram -> TypeCheckResult ()
semanticAnalysis program = do
  runReaderT (runStateT (semanticAnalysis' program) Data.Map.empty) Data.Map.empty
  return ()

-- semanticAnalysis' :: Env -> Store -> TProgram -> TypeCheckResult ()
-- semanticAnalysis' env store program = do
semanticAnalysis' :: TProgram -> PartialResult ()
semanticAnalysis' (Program _ dfs) = do
--   env <- ask
--   store <- get
  env <- checkFunctionSignatures dfs
  -- todo save function signatures
  -- todo (^and) repeated argument name
  -- todo check if main exists
  -- todo check functions body
  return ()

checkFunctionSignatures :: [TTopDef] -> PartialResult Env
checkFunctionSignatures [] = do
  env <- ask
  -- check if main exists
  case (Data.Map.lookup (Ident "main") env) of
    Nothing -> throwError $ ":1:1: missing 'main' function"
    (Just loc) -> do
      state <- get
      case (Data.Map.lookup loc state) of
        (Just (Fun _ (Int _) [])) -> return env
        (Just (Fun pos (Int _) _)) -> throwError $ tokenPos pos ++ " main function must have no argument"
        (Just (Fun pos _ [])) -> throwError $ tokenPos pos ++ " main function must return integer"
        otherwise -> throwError ":1:1: Internal memory error"

checkFunctionSignatures ((FnDef pos retType fname args _):dfs) = do
  env <- ask
  case (Data.Map.lookup fname env) of
    Nothing -> do
      argTypes <- return $ getArgTypes args
--       if checkArgNames args then throwError ""
      checkArgNames args
      ftype <- return $ Fun pos retType argTypes
      env' <- declare fname ftype
      local (const env') $ checkFunctionSignatures dfs
    otherwise -> throwError $ tokenPos pos ++ " function name '" ++ (showVarName fname) ++ "' already in use."

declare :: Ident -> TType -> PartialResult Env
declare varName varType = do
  env <- ask
  loc <- newloc
  modify $ Data.Map.insert loc (varType)
  return $ Data.Map.insert varName loc env


newloc :: PartialResult Loc
newloc = do
  store <- get
  return $ hNewloc 0 store

hNewloc :: Loc -> Store -> Loc
hNewloc loc store =
  case Data.Map.lookup loc store of
    Nothing -> loc
    otherwise -> hNewloc (loc + 1) store

showVarName :: Ident -> String
showVarName (Ident fname) = fname

getArgTypes :: [TArg] -> [TType]
getArgTypes args = reverse $ getArgTypes' args [] where
  getArgTypes' [] acc = acc
  getArgTypes' ((Arg _ argType _):args) acc = getArgTypes' args (argType:acc)

checkArgNames :: [TArg] -> PartialResult Env
checkArgNames args = checkArgNames' args Data.Set.empty

checkArgNames' :: [TArg] -> (Data.Set.Set String) -> PartialResult Env
checkArgNames' [] _ = ask
checkArgNames' ((Arg pos _ (Ident name)):args) names
  | Data.Set.member name names = throwError $ tokenPos pos ++ " repeated argument name '" ++ name ++ "'"
  | otherwise = checkArgNames' args (Data.Set.insert name names)

tokenPos :: Maybe (Int, Int) -> String
tokenPos Nothing = "\n Something went wrong \n"
tokenPos (Just (r,c)) = ":" ++ (show r) ++ ":" ++ (show c) ++ ":"