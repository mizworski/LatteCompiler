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
  runReaderT (runStateT (semanticAnalysis' program) Data.Map.empty)
             (Env {vars = Data.Map.empty, usedNames = Data.Set.empty})
  return ()


semanticAnalysis' :: TProgram -> PartialResult ()
semanticAnalysis' (Program _ dfs) = do
  env <- checkFunctionSignatures dfs
  local (const env) $ checkFunctionsBody dfs
  -- todo checkExpressionType
  -- todo check undeclared variables
  -- todo non reachable / always reachable branches
  -- todo check if correct argument types
  -- todo check operations types
  -- todo built in fns
  -- todo check number of arguments
  -- todo comparison types

--   return ()

checkFunctionsBody :: [TTopDef] -> PartialResult ()
checkFunctionsBody [] = return()
-- checkFunctionsBody ((FnDef pos retType _ args (Block _ [])):dfs) = throwError $ tokenPos pos ++ " empty function."
checkFunctionsBody ((FnDef pos retType _ args (Block _ stmts)):dfs) = do
  env <- declareArguments args
  local (const env) $ checkStatements pos retType stmts
  checkFunctionsBody dfs

checkStatements :: SPos -> TType -> [TStmt] -> PartialResult ()
-- todo function 'f' has no return
checkStatements pos _ [] = throwError $ tokenPos pos ++ " function has no return"
checkStatements pos retType (stmt:stmts) = do
  res <- checkStatement stmt retType
  case res of
    (Just env) -> local (const env) $ checkStatements pos retType stmts
    -- function returned
    otherwise -> return ()

checkStatement :: TStmt -> TType ->  PartialResult (Maybe Env)
checkStatement (VRet pos) retType = do
  case retType of
    (Void _) -> return Nothing -- Nothing = Function returned
    (Int pos) -> throwError $ tokenPos pos ++ " Couldn't match actual type 'Void' with expected 'Int'"
    (Str pos) -> throwError $ tokenPos pos ++ " Couldn't match actual type 'Void' with expected 'Str'"
    (Bool pos) -> throwError $ tokenPos pos ++ " Couldn't match actual type 'Void' with expected 'Bool'"
    (Fun pos rtype atypes) -> throwError $ tokenPos pos ++ " Couldn't match actual type 'Void' with expected '" ++
                              (show $ Fun pos rtype atypes) ++ "'"

checkStatement (Ret pos expr) retType = do
  actualType <- checkType expr
  case (actualType == retType) of
    True -> return Nothing
    otherwise -> do
      case actualType of
        (Int pos) -> throwError $ tokenPos pos ++ " Couldn't match actual type 'Int' with expected '"
                                               ++ (show retType) ++ "'"
        (Str pos) -> throwError $ tokenPos pos ++ " Couldn't match actual type 'Str' with expected '"
                                               ++ (show retType) ++ "'"
        (Bool pos) -> throwError $ tokenPos pos ++ " Couldn't match actual type 'Bool' with expected '"
                                                ++ (show retType) ++ "'"
        (Fun pos rtype atypes) -> throwError $ tokenPos pos ++ " Couldn't match actual type '"
                                                            ++ (show $ Fun pos rtype atypes)
                                                            ++"' with expected '" ++ (show retType) ++ "'"

        --todo
        otherwise -> throwError $ tokenPos pos ++ " incorrect return type"
        -- not checking void type as checkType should never return void type

checkStatement (Empty _) _ = do
  env <- ask
  return $ Just env

checkStatement (BStmt _ (Block bpos stmts)) retType = do
  env <- ask
  -- todo can override function name then
  env' <- return $ Env {vars = vars env, usedNames = Data.Set.empty}
  local (const env') $ checkStatements bpos retType stmts
  return $ Just env

-- todo are items nonempty? [int;]
checkStatement (Decl dpos dtype []) _ = do
  env <- ask
  return $ Just env

checkStatement (Decl dpos dtype ((NoInit ipos ident):items)) rtype = do
  env <- ask
  case (Data.Set.member ident (usedNames env)) of
    True -> throwError $ tokenPos ipos ++ " variable name already declared in this block"
    otherwise -> do
      env' <- local (const env) $ declare ident dtype
      local (const env') $ checkStatement (Decl dpos dtype items) rtype

checkStatement (Decl dpos dtype ((Init ipos ident expr):items)) rtype = do
  actualType <- checkType expr
  case (actualType == dtype) of
    True -> checkStatement (Decl dpos dtype ((NoInit ipos ident):items)) rtype
    otherwise -> throwError $ tokenPos ipos ++ " couldn't match actual type '" ++ (show actualType)
                                            ++ "' with expected '" ++ (show dtype) ++ "'"

checkStatement (Ass apos ident expr) retType = do
  env <- ask
  case (Data.Map.member ident (vars env)) of
    False -> throwError $ tokenPos apos ++ " variable '" ++ (showVarName ident) ++ "' not declared"
    otherwise -> do
      actualType <- checkType expr
      state <- get
      (Just loc) <- return $ Data.Map.lookup ident (vars env)
      (Just expectedType) <- return $ Data.Map.lookup loc state
      case (actualType == expectedType) of
        True -> return $ Just env
        otherwise -> throwError $ tokenPos apos ++ " couldn't match actual type '" ++ (show actualType)
                                                ++ "' with expected '" ++ (show expectedType) ++ "'"

checkStatement (Incr pos ident) _ = checkIncDec pos ident "incremented"
checkStatement (Decr pos ident) _ = checkIncDec pos ident "decremented"

checkStatement stmt retType = do
  env <- ask
  return $ Just env


checkIncDec :: SPos -> Ident -> String -> PartialResult (Maybe Env)
checkIncDec pos ident incdec = do
  env <- ask
  case (Data.Map.member ident (vars env)) of
    False -> throwError $ tokenPos pos ++ " variable '" ++ (showVarName ident) ++ "' not declared"
    otherwise -> do
      state <- get
      (Just loc) <- return $ Data.Map.lookup ident (vars env)
      (Just vtype) <- return $ Data.Map.lookup loc state
      case vtype of
        (Int _) -> return $ Just env
        otherwise -> throwError $ tokenPos pos ++ " only integers can be " ++ incdec ++ ", got '" ++ (show vtype) ++ "'"

checkType :: TExpr -> PartialResult TType
checkType expr = return $ Int (Just (0,0))


checkFunctionSignatures :: [TTopDef] -> PartialResult Env
checkFunctionSignatures [] = do
  env <- ask
  case (Data.Map.lookup (Ident "main") (vars env)) of
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
  case (Data.Map.lookup fname (vars env)) of
    Nothing -> do
      argTypes <- return $ getArgTypes args
      checkArgNames args
      ftype <- return $ Fun pos retType argTypes
      env' <- declare fname ftype
      local (const env') $ checkFunctionSignatures dfs
    otherwise -> throwError $ tokenPos pos ++ " function name '" ++ (showVarName fname) ++ "' already in use."


declareArguments :: [TArg] -> PartialResult Env
declareArguments [] = ask
declareArguments ((Arg _ argtype argname):args) = do
  env <- ask
  env' <- declare argname argtype
  declareArguments args


declare :: Ident -> TType -> PartialResult Env
declare varName varType = do
  env <- ask
  loc <- newloc
  modify $ Data.Map.insert loc (varType)
  return $ Env {vars = Data.Map.insert varName loc (vars env), usedNames = Data.Set.insert varName (usedNames env)}


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
