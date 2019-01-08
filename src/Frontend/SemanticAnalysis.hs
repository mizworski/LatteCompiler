module Frontend.SemanticAnalysis where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map
import qualified Data.Set

import Frontend.AbsLatte
import Frontend.Datatypes

semanticAnalysis :: TProgram -> TypeCheckResult ()
semanticAnalysis program = do
  runReaderT (runStateT (semanticAnalysis' program) Data.Map.empty)
             (Env {vars = Data.Map.empty, usedNames = Data.Set.empty, computationStatus = Running})
  return ()

semanticAnalysis' :: TProgram -> PartialResult ()
semanticAnalysis' (Program _ dfs) = do
  env <- declareBuiltInFns
  env' <- local (const env) $ checkFunctionSignatures dfs
  local (const env') $ checkFunctionsBody dfs

checkFunctionsBody :: [TTopDef] -> PartialResult ()
checkFunctionsBody [] = return()
checkFunctionsBody ((FnDef pos retType fname args (Block _ stmts)):dfs) = do
  env <- declareArguments args
  env' <- local (const env) $ checkStatements pos retType stmts
  case (computationStatus env') of
    Running -> do
      case (retType == Void (Just (0,0))) of
        True -> checkFunctionsBody dfs
        otherwise -> throwError $ tokenPos pos ++ " function '" ++ (showVarName fname) ++ "' has no return"
    otherwise -> checkFunctionsBody dfs

checkStatements :: SPos -> TType -> [TStmt] -> PartialResult Env
checkStatements pos _ [] = ask
checkStatements pos retType (stmt:stmts) = do
  env <- checkStatement stmt retType
  case (computationStatus env) of
    Ended -> return env
    otherwise -> local (const env) $ checkStatements pos retType stmts

checkStatement :: TStmt -> TType ->  PartialResult Env
checkStatement (Empty _) _ = ask

checkStatement (BStmt _ (Block bpos stmts)) retType = do
  env <- ask
  env' <- return Env {vars = vars env, usedNames = Data.Set.empty, computationStatus = computationStatus env}
  env'' <- local (const env') $ checkStatements bpos retType stmts
  return Env {vars = vars env, usedNames = usedNames env, computationStatus = computationStatus env''}

checkStatement (Decl dpos dtype []) _ = ask
checkStatement (Decl dpos dtype ((NoInit ipos ident):items)) rtype = do
  case (dtype == Void (Just (0,0))) of
    True -> throwError $ tokenPos ipos ++ " can't declare void type"
    otherwise -> do
      env <- ask
      case (Data.Set.member ident (usedNames env)) of
        True -> throwError $ tokenPos ipos ++ " variable name already declared in this block"
        otherwise -> do
          case (Data.Map.lookup ident (vars env)) of
            (Just loc) -> do
              state <- get
              (Just vtype) <- return $ Data.Map.lookup loc state
              case vtype of
                (Fun _ _ _) -> throwError $ tokenPos ipos ++ " name '" ++ (showVarName ident) ++ "' already in use"
                otherwise -> do
                  env' <- local (const env) $ declare ident dtype
                  local (const env') $ checkStatement (Decl dpos dtype items) rtype
            otherwise -> do
              env' <- local (const env) $ declare ident dtype
              local (const env') $ checkStatement (Decl dpos dtype items) rtype

checkStatement (Decl dpos dtype ((Init ipos ident expr):items)) rtype = do
  case (dtype == Void (Just (0,0))) of
    True -> throwError $ tokenPos ipos ++ " can't declare void type"
    otherwise -> do
      actualType <- checkType expr
      case (actualType == dtype) of
        True -> checkStatement (Decl dpos dtype ((NoInit ipos ident):items)) rtype
        otherwise -> throwError $ tokenPos (getPos actualType) ++ " couldn't match actual type '" ++ (show actualType)
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
      -- todo prohibit actualType == void? checkType should never allow it anyway 
      case (actualType == expectedType) of
        True -> return env
        otherwise -> throwError $ tokenPos (getPos actualType) ++ " assigning expression of type '" ++ (show actualType)
                                                               ++ "' to variable of type '" ++ (show expectedType) ++ "'"

checkStatement (Incr pos ident) _ = checkIncDec pos ident "incremented"
checkStatement (Decr pos ident) _ = checkIncDec pos ident "decremented"

checkStatement (Ret _ expr) retType = do
  env <- ask
  actualType <- checkType expr
  case (actualType == retType) of
    True -> do
      case actualType of
        (Void pos) -> throwError $ tokenPos pos ++ " can't return expression value from void function"
        otherwise -> return Env {vars = vars env, usedNames = usedNames env, computationStatus = Ended}
    otherwise -> do
      case actualType of
        (Int pos) -> throwError $ tokenPos pos ++ " couldn't match actual type 'Int' with expected '"
                                               ++ (show retType) ++ "'"
        (Str pos) -> throwError $ tokenPos pos ++ " couldn't match actual type 'Str' with expected '"
                                               ++ (show retType) ++ "'"
        (Bool pos) -> throwError $ tokenPos pos ++ " couldn't match actual type 'Bool' with expected '"
                                                ++ (show retType) ++ "'"
        (Fun pos rtype atypes) -> throwError $ tokenPos pos ++ " couldn't match actual type '"
                                                            ++ (show $ Fun pos rtype atypes)
                                                            ++"' with expected '" ++ (show retType) ++ "'"
        (Void pos) -> throwError $ tokenPos pos ++ " trying to return expression with void type"

checkStatement (VRet _) retType = do
  case retType of
    (Void _) -> do
      env <- ask
      return $ Env {vars = vars env, usedNames = usedNames env, computationStatus = Ended}
    (Int pos) -> throwError $ tokenPos pos ++ " couldn't match actual type 'Void' with expected 'Int'"
    (Str pos) -> throwError $ tokenPos pos ++ " couldn't match actual type 'Void' with expected 'Str'"
    (Bool pos) -> throwError $ tokenPos pos ++ " couldn't match actual type 'Void' with expected 'Bool'"
    (Fun pos rtype atypes) -> throwError $ tokenPos pos ++ " couldn't match actual type 'Void' with expected '" ++
                              (show $ Fun pos rtype atypes) ++ "'"

checkStatement (SExp _ expr) _ = do
  checkType expr
  ask

checkStatement (Cond pos bexpr ifTrueStmt) retType = do
  env <- ask
  res <- checkBoolConstexpr bexpr pos
  case res of
    (Just False) -> return env
    (Just True) -> do
      env' <- local (const env) $ checkStatement ifTrueStmt retType
      return Env {vars = vars env, usedNames = usedNames env, computationStatus = computationStatus env'}
    otherwise -> do
      env' <- local (const env) $ checkStatement ifTrueStmt retType
      case (computationStatus env) of
        Running -> return env
        otherwise -> return Env {vars = vars env, usedNames = usedNames env, computationStatus = MaybeEnded}

checkStatement (CondElse pos bexpr ifTrueStmt ifFalseStmt) retType = do
  env <- ask
  res <- checkBoolConstexpr bexpr pos
  case res of
    (Just False) -> do
      env' <- local (const env) $ checkStatement ifFalseStmt retType
      return Env {vars = vars env, usedNames = usedNames env, computationStatus = computationStatus env'}
    (Just True) -> do
      env' <- local (const env) $ checkStatement ifTrueStmt retType
      return Env {vars = vars env, usedNames = usedNames env, computationStatus = computationStatus env'}
    otherwise -> do
      envTrue <- local (const env) $ checkStatement ifTrueStmt retType
      envFalse <- local (const env) $ checkStatement ifFalseStmt retType
      case (computationStatus envTrue) of
        Running -> do
          case (computationStatus envFalse) of
            Running -> return env
            otherwise -> do
              return Env {vars = vars env, usedNames = usedNames env, computationStatus = MaybeEnded}
        Ended -> do
          case (computationStatus envFalse) of
            Ended -> do
              return Env {vars = vars env, usedNames = usedNames env, computationStatus = Ended}
            otherwise -> do
              return Env {vars = vars env, usedNames = usedNames env, computationStatus = MaybeEnded}
        MaybeEnded -> do
          return Env {vars = vars env, usedNames = usedNames env, computationStatus = MaybeEnded}

checkStatement (While pos bexpr loopBody) retType = do
  env <- ask
  res <- checkBoolConstexpr bexpr pos
  case res of
    (Just False) -> return env
    (Just True) -> do
      env' <- local (const env) $ checkStatement loopBody retType
      case (computationStatus env') of
        Running -> throwError $ tokenPos pos ++ " infinite loop"
        otherwise -> return Env {vars = vars env, usedNames = usedNames env, computationStatus = Ended}
    otherwise -> do
      env' <- checkStatement loopBody retType
      case (computationStatus env') of
        Running -> return Env {vars = vars env, usedNames = usedNames env, computationStatus = Running}
        otherwise -> return Env {vars = vars env, usedNames = usedNames env, computationStatus = MaybeEnded}

checkBoolConstexpr :: TExpr -> SPos -> PartialResult (Maybe Bool)
-- todo simplify more?
checkBoolConstexpr (ELitTrue _) _ = return $ Just True
checkBoolConstexpr (ELitFalse _) _ = return $ Just False
checkBoolConstexpr expr pos = do
  exprType <- checkType expr
  case (exprType == Bool (Just (0,0))) of
    True -> return Nothing
    otherwise -> throwError $ tokenPos pos ++ " 'if' condition must be boolean expression, got '"
                                           ++ (show exprType) ++ "'"

checkIncDec :: SPos -> Ident -> String -> PartialResult Env
checkIncDec pos ident incdec = do
  env <- ask
  case (Data.Map.member ident (vars env)) of
    False -> throwError $ tokenPos pos ++ " variable '" ++ (showVarName ident) ++ "' not declared"
    otherwise -> do
      state <- get
      (Just loc) <- return $ Data.Map.lookup ident (vars env)
      (Just vtype) <- return $ Data.Map.lookup loc state
      case vtype of
        (Int _) -> return env
        otherwise -> throwError $ tokenPos pos ++ " only integers can be " ++ incdec ++ ", got '" ++ (show vtype) ++ "'"

checkType :: TExpr -> PartialResult TType
checkType (EVar pos ident) = do
  env <- ask
  case (Data.Map.member ident (vars env)) of
    False -> throwError $ tokenPos pos ++ " variable '" ++ (showVarName ident) ++ "' not declared"
    otherwise -> do
      state <- get
      (Just loc) <- return $ Data.Map.lookup ident (vars env)
      (Just vtype) <- return $ Data.Map.lookup loc state
      -- change pos from where var was declared to where it is called
      return $ changePos vtype pos

checkType (ELitInt pos num) = do
  case (num > 2147483647) of
    True -> throwError $ tokenPos pos ++ " value larger than MAX_INT"
    otherwise -> do
      case (num < -2147483648) of
        True -> throwError $ tokenPos pos ++ " value lower than MIN_INT"
        otherwise -> return $ Int pos

checkType (ELitTrue pos) = return $ Bool pos
checkType (ELitFalse pos) = return $ Bool pos
checkType (EApp pos ident args) = do
  env <- ask
  case (Data.Map.member ident (vars env)) of
    False -> throwError $ tokenPos pos ++ " function '" ++ (showVarName ident) ++ "' not declared"
    otherwise -> do
      state <- get
      (Just loc) <- return $ Data.Map.lookup ident (vars env)
      (Just (Fun _ retType argTypes)) <- return $ Data.Map.lookup loc state
      checkParametersPassed args argTypes ident pos
      return $ changePos retType pos

checkType (EString pos _) = return $ Str pos

checkType (Neg pos expr) = do
  actualType <- checkType expr
  case (actualType == Int (Just (0,0))) of
    True -> return $ Int pos
    otherwise -> throwError " operator '-' can be used only in front of integer expressions"

checkType (Not pos expr) = do
  actualType <- checkType expr
  case (actualType == Bool (Just (0,0))) of
    True -> return $ Bool pos
    otherwise -> throwError " operator '!' can be used only in front of boolean expressions"

checkType (EMul pos expr1 op expr2) = do
  actualType <- checkType expr1
  case (actualType == Int (Just (0,0))) of
    True -> do
      checkOperandTypes expr1 expr2
      return $ Int pos
    otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can be performend only on integer operands"

checkType (EAdd pos expr1 op expr2) = do
  actualType <- checkType expr1
  case (actualType == Int (Just (0,0))) of
    True -> do
      checkOperandTypes expr1 expr2
      return $ Int pos
    otherwise -> do
      case (actualType == Str (Just (0,0)) && op == Plus (Just (0,0))) of
        True -> do
          checkOperandTypes expr1 expr2
          return $ Str pos
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"
                                               ++ (show actualType) ++ "' operand"

checkType (ERel pos expr1 op expr2) = do
  actualType <- checkType expr1
  case op of
    -- todo how to simplify this copy-paste?
    (GE _) -> do
      case actualType of
        (Int _) -> do
          checkOperandTypes expr1 expr2
          return $ Bool pos
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"
                                               ++ (show actualType) ++ "' operand"
    (GTH _) -> do
      case actualType of
        (Int _) -> do
          checkOperandTypes expr1 expr2
          return $ Bool pos
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"
                                               ++ (show actualType) ++ "' operand"
    (LE _) -> do
      case actualType of
        (Int _) -> do
          checkOperandTypes expr1 expr2
          return $ Bool pos
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"
                                               ++ (show actualType) ++ "' operand"
    (LTH _) -> do
      case actualType of
        (Int _) -> do
          checkOperandTypes expr1 expr2
          return $ Bool pos
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"
                                               ++ (show actualType) ++ "' operand"
    otherwise -> do
      -- EQ / NEQ only on Int, Bool and Str
      case actualType of
        (Int _) -> do
          checkOperandTypes expr1 expr2
          return $ Bool pos
        (Bool _) -> do
          checkOperandTypes expr1 expr2
          return $ Bool pos
        (Str _) -> do
          checkOperandTypes expr1 expr2
          return $ Bool pos
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"
                                                      ++ (show actualType) ++ "' operand"

checkType (EAnd pos expr1 expr2) = do
  actualType <- checkType expr1
  case (actualType == Bool (Just (0,0))) of
    True -> do
      checkOperandTypes expr1 expr2
      return $ Bool pos
    otherwise -> throwError $ tokenPos pos ++ " logical 'and' can be performend only on boolean operands."

checkType (EOr pos expr1 expr2) = do
  actualType <- checkType expr1
  case (actualType == Bool (Just (0,0))) of
    True -> do
      checkOperandTypes expr1 expr2
      return $ Bool pos
    otherwise -> throwError $ tokenPos pos ++ " logical 'or' can be performend only on boolean operands."

checkOperandTypes :: TExpr -> TExpr -> PartialResult ()
checkOperandTypes expr1 expr2 = do
  actualType1 <- checkType expr1
  actualType2 <- checkType expr2
  case (actualType1 == actualType2) of
    False -> throwError $ tokenPos (getPos actualType1) ++ " operands types mismatch - left type: '" ++ (show actualType1)
                                                        ++ "', right type: '" ++ (show actualType2) ++ "'"
    otherwise -> return ()

declareBuiltInFns :: PartialResult Env
declareBuiltInFns = do
  pos <- return $ Just (0,0)
  env <- declare (Ident "printInt") (Fun pos (Void pos) [(Int pos)])
  env' <- local (const env) $ declare (Ident "printString") (Fun pos (Void pos) [(Str pos)])
  env'' <- local (const env') $ declare (Ident "error") (Fun pos (Void pos) [])
  env''' <- local (const env'') $ declare (Ident "readInt") (Fun pos (Int pos) [])
  env'''' <- local (const env''') $ declare (Ident "readString") (Fun pos (Str pos) [])
  env''''' <- local (const env'''') $ declare (Ident "__concat") (Fun pos (Str pos) [(Str pos), (Str pos)])
  return env'''''

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
      checkArgsDefinition args
      ftype <- return $ Fun pos retType argTypes
      env' <- declare fname ftype
      local (const env') $ checkFunctionSignatures dfs
    otherwise -> throwError $ tokenPos pos ++ " function name '" ++ (showVarName fname) ++ "' already in use."

declareArguments :: [TArg] -> PartialResult Env
declareArguments [] = ask
declareArguments ((Arg _ argtype argname):args) = do
  env <- declare argname argtype
  local (const env) $ declareArguments args

declare :: Ident -> TType -> PartialResult Env
declare varName varType = do
  env <- ask
  loc <- newloc
  modify $ Data.Map.insert loc varType
  return $ Env { vars = Data.Map.insert varName loc (vars env)
               , usedNames = Data.Set.insert varName (usedNames env)
               , computationStatus = computationStatus env}

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

checkArgsDefinition :: [TArg] -> PartialResult Env
checkArgsDefinition args = checkArgsDefinition' args Data.Set.empty

checkArgsDefinition' :: [TArg] -> (Data.Set.Set String) -> PartialResult Env
checkArgsDefinition' [] _ = ask
checkArgsDefinition' ((Arg pos atype (Ident name)):args) names
  | Data.Set.member name names = throwError $ tokenPos pos ++ " repeated argument name '" ++ name ++ "'"
  | otherwise = do
    case (atype == Void (Just (0,0))) of
      True -> throwError $ tokenPos pos ++ " argument '" ++ name ++ "' can't be void"
      otherwise -> checkArgsDefinition' args (Data.Set.insert name names)

checkParametersPassed :: [TExpr] -> [TType] -> Ident -> SPos -> PartialResult Env
checkParametersPassed [] [] _ _ = ask
checkParametersPassed [] _ ident pos = throwError $ tokenPos pos ++ " not enough arguments passed to function '"
                                                    ++ (showVarName ident) ++ "'"
checkParametersPassed _ [] ident pos = throwError $ tokenPos pos ++ " too many arguments passed to function '"
                                                    ++ (showVarName ident) ++ "'"
checkParametersPassed (expr:exprs) (expectedType:argTypes) ident pos = do
  actualType <- checkType expr
  case (actualType == expectedType) of
    True -> checkParametersPassed exprs argTypes ident pos
    otherwise -> throwError $ tokenPos (getPos actualType) ++ " couldn't match actual type '" ++ (show actualType)
                                                           ++ "' with expected '" ++ (show expectedType) ++ "'"

tokenPos :: Maybe (Int, Int) -> String
tokenPos Nothing = "\n Something went wrong \n"
tokenPos (Just (r,c)) = ":" ++ (show r) ++ ":" ++ (show c) ++ ":"

getPos :: TType -> Maybe (Int, Int)
getPos (Int pos) = pos
getPos (Str pos) = pos
getPos (Bool pos) = pos
getPos (Void pos) = pos
getPos (Fun pos _ _) = pos

changePos :: TType -> SPos -> TType
changePos (Int _) pos = Int pos
changePos (Str _) pos = Str pos
changePos (Bool _) pos = Bool pos
changePos (Void _) pos = Void pos
changePos (Fun _ rtype atypes) pos = Fun pos rtype atypes
