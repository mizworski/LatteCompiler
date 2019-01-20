module Frontend.SemanticAnalysis where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map
import qualified Data.Set

import Frontend.AbsLatte
import Frontend.Datatypes

semanticAnalysis :: TProgram -> TypeCheckResult TProgram
semanticAnalysis program = do
  (program', _) <- runReaderT (runStateT (semanticAnalysis' program) Data.Map.empty)
                              (Env {vars = Data.Map.empty, usedNames = Data.Set.empty, computationStatus = Running})
  return program'

semanticAnalysis' :: TProgram -> PartialResult TProgram
semanticAnalysis' (Program pos dfs) = do
  env <- declareBuiltInFns
  env' <- local (const env) $ checkFunctionSignatures dfs
  dfs' <- local (const env') $ checkFunctionsBody dfs
  return (Program pos dfs')

checkFunctionsBody :: [TTopDef] -> PartialResult [TTopDef]
checkFunctionsBody [] = return []
checkFunctionsBody ((FnDef pos retType fname args (Block pos2 stmts)):dfs) = do
  env <- declareArguments args
  (env', stmts') <- local (const env) $ checkStatements pos retType stmts
  case (computationStatus env') of
    Running -> do
      case (retType == Void (Just (0,0))) of
        True -> do
          dfs' <- checkFunctionsBody dfs
          return $ (FnDef pos retType fname args (Block pos2 stmts')):dfs'
        otherwise -> throwError $ tokenPos pos ++ " function '" ++ (showVarName fname) ++ "' has no return"
    otherwise -> do
      dfs' <- checkFunctionsBody dfs
      return $ (FnDef pos retType fname args (Block pos2 stmts')):dfs'

checkStatements :: SPos -> TType -> [TStmt] -> PartialResult (Env, [TStmt])
checkStatements pos _ [] = do
  env <- ask
  return (env, [])
checkStatements pos retType (stmt:stmts) = do
  (env, stmt') <- checkStatement stmt retType
  case (computationStatus env) of
    Ended -> return (env, [stmt'])
    otherwise -> do
      (env', stmts') <- local (const env) $ checkStatements pos retType stmts
      return (env', stmt':stmts')

checkStatement :: TStmt -> TType ->  PartialResult (Env, TStmt)
checkStatement (Empty pos) _ = do
  env <- ask
  return (env, Empty pos)

checkStatement (BStmt pos (Block bpos stmts)) retType = do
  env <- ask
  env' <- return Env {vars = vars env, usedNames = Data.Set.empty, computationStatus = computationStatus env}
  (env'', stmts') <- local (const env') $ checkStatements bpos retType stmts
  env''' <- return Env {vars = vars env, usedNames = usedNames env, computationStatus = computationStatus env''}
  return (env''', BStmt pos (Block bpos stmts'))

checkStatement (Decl dpos dtype []) _ = do
  env <- ask
  return (env, Decl dpos dtype [])
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
                  (env'', Decl _ _ items') <- local (const env') $ checkStatement (Decl dpos dtype items) rtype
                  return (env'', Decl dpos dtype ((NoInit ipos ident):items'))
            otherwise -> do
              env' <- local (const env) $ declare ident dtype
              (env'', Decl _ _ items') <- local (const env') $ checkStatement (Decl dpos dtype items) rtype
              return (env'', Decl dpos dtype ((NoInit ipos ident):items'))
checkStatement (Decl dpos dtype ((Init ipos ident expr):items)) rtype = do
  case (dtype == Void (Just (0,0))) of
    True -> throwError $ tokenPos ipos ++ " can't declare void type"
    otherwise -> do
      (actualType, expr') <- checkType expr
      case (actualType == dtype) of
        True -> do
          (env', Decl _ _ (_:items')) <- checkStatement (Decl dpos dtype ((NoInit ipos ident):items)) rtype
          return (env', Decl dpos dtype ((Init ipos ident expr'):items'))
        otherwise -> throwError $ tokenPos (getPos actualType) ++ " couldn't match actual type '" ++ (show actualType)
                                                           ++ "' with expected '" ++ (show dtype) ++ "'"

checkStatement (Ass apos ident expr) retType = do
  env <- ask
  case (Data.Map.member ident (vars env)) of
    False -> throwError $ tokenPos apos ++ " variable '" ++ (showVarName ident) ++ "' not declared"
    otherwise -> do
      (actualType, expr') <- checkType expr
      state <- get
      (Just loc) <- return $ Data.Map.lookup ident (vars env)
      (Just expectedType) <- return $ Data.Map.lookup loc state
      -- todo prohibit actualType == void? checkType should never allow it anyway 
      case (actualType == expectedType) of
        True -> return (env, Ass apos ident expr')
        otherwise -> throwError $ tokenPos (getPos actualType) ++ " assigning expression of type '" ++ (show actualType)
                                                               ++ "' to variable of type '" ++ (show expectedType) ++ "'"

checkStatement (Incr pos ident) _ = do
  env <- checkIncDec pos ident "incremented"
  return (env, Incr pos ident)
checkStatement (Decr pos ident) _ = do
  env <- checkIncDec pos ident "decremented"
  return (env, Decr pos ident)
checkStatement (Ret pos expr) retType = do
  env <- ask
  (actualType, expr') <- checkType expr
  case (actualType == retType) of
    True -> do
      case actualType of
        (Void pos) -> throwError $ tokenPos pos ++ " can't return expression value from void function"
        otherwise -> return (Env {vars = vars env, usedNames = usedNames env, computationStatus = Ended}, Ret pos expr')
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

checkStatement (VRet pos) retType = do
  case retType of
    (Void _) -> do
      env <- ask
      return $ (Env {vars = vars env, usedNames = usedNames env, computationStatus = Ended}, VRet pos)
    (Int pos) -> throwError $ tokenPos pos ++ " couldn't match actual type 'Void' with expected 'Int'"
    (Str pos) -> throwError $ tokenPos pos ++ " couldn't match actual type 'Void' with expected 'Str'"
    (Bool pos) -> throwError $ tokenPos pos ++ " couldn't match actual type 'Void' with expected 'Bool'"
    (Fun pos rtype atypes) -> throwError $ tokenPos pos ++ " couldn't match actual type 'Void' with expected '" ++
                              (show $ Fun pos rtype atypes) ++ "'"

checkStatement (SExp pos expr) _ = do
  (_, expr') <- checkType expr
  env <- ask
  return (env, SExp pos expr')

checkStatement (Cond pos bexpr ifTrueStmt) retType = do
  env <- ask
  (res, bexpr') <- checkBoolConstexpr bexpr pos
--   throwError $ show res
  case res of
    (Just False) -> return (env, Empty pos)
    (Just True) -> do
      (env', ifTrueStmt') <- local (const env) $ checkStatement ifTrueStmt retType
      return (Env {vars = vars env, usedNames = usedNames env, computationStatus = computationStatus env'}, ifTrueStmt')
    otherwise -> do
      (env', ifTrueStmt') <- local (const env) $ checkStatement ifTrueStmt retType
      case (computationStatus env) of
        Running -> return (env, Cond pos bexpr' ifTrueStmt')
        otherwise -> return (Env {vars = vars env, usedNames = usedNames env, computationStatus = MaybeEnded},
                             Cond pos bexpr' ifTrueStmt')

checkStatement (CondElse pos bexpr ifTrueStmt ifFalseStmt) retType = do
  env <- ask
  (res, bexpr') <- checkBoolConstexpr bexpr pos
  case res of
    (Just False) -> do
      (env', ifFalseStmt') <- local (const env) $ checkStatement ifFalseStmt retType
      return (Env {vars = vars env, usedNames = usedNames env, computationStatus = computationStatus env'}, ifFalseStmt')
    (Just True) -> do
      (env', ifTrueStmt') <- local (const env) $ checkStatement ifTrueStmt retType
      return (Env {vars = vars env, usedNames = usedNames env, computationStatus = computationStatus env'}, ifTrueStmt')
    otherwise -> do
      (envTrue, ifTrueStmt') <- local (const env) $ checkStatement ifTrueStmt retType
      (envFalse, ifFalseStmt') <- local (const env) $ checkStatement ifFalseStmt retType
      case (computationStatus envTrue) of
        Running -> do
          case (computationStatus envFalse) of
            Running -> return (env, CondElse pos bexpr' ifTrueStmt' ifFalseStmt')
            otherwise -> do
              return (Env {vars = vars env, usedNames = usedNames env, computationStatus = MaybeEnded},
                      CondElse pos bexpr' ifTrueStmt' ifFalseStmt')
        Ended -> do
          case (computationStatus envFalse) of
            Ended -> do
              return (Env {vars = vars env, usedNames = usedNames env, computationStatus = Ended},
                      CondElse pos bexpr' ifTrueStmt' ifFalseStmt')
            otherwise -> do
              return (Env {vars = vars env, usedNames = usedNames env, computationStatus = MaybeEnded},
                      CondElse pos bexpr' ifTrueStmt' ifFalseStmt')
        MaybeEnded -> do
          return (Env {vars = vars env, usedNames = usedNames env, computationStatus = MaybeEnded},
                  CondElse pos bexpr' ifTrueStmt' ifFalseStmt')

checkStatement (While pos bexpr loopBody) retType = do
  env <- ask
  (res, bexpr') <- checkBoolConstexpr bexpr pos
  case res of
    (Just False) -> return (env, Empty pos)
    (Just True) -> do
      (env', loopBody') <- local (const env) $ checkStatement loopBody retType
      case (computationStatus env') of
        Running -> throwError $ tokenPos pos ++ " infinite loop"
        otherwise -> return (Env {vars = vars env, usedNames = usedNames env, computationStatus = Ended},
                             While pos bexpr' loopBody')
    otherwise -> do
      (env', loopBody') <- checkStatement loopBody retType
      case (computationStatus env') of
        Running -> return (Env {vars = vars env, usedNames = usedNames env, computationStatus = Running},
                           While pos bexpr' loopBody')
        otherwise -> return (Env {vars = vars env, usedNames = usedNames env, computationStatus = MaybeEnded},
                             While pos bexpr' loopBody')

checkBoolConstexpr :: TExpr -> SPos -> PartialResult (Maybe Bool, TExpr)
checkBoolConstexpr expr pos = do
  (exprType, expr') <- checkType expr
  case expr' of
    (ELitTrue _) -> return (Just True, ELitTrue pos)
    (ELitFalse _) -> return (Just False, ELitFalse pos)
    otherwise -> do
      case (exprType == Bool (Just (0, 0))) of
        True -> return (Nothing, expr')
        False -> throwError $ tokenPos pos ++ " 'if' condition must be boolean expression, got '"
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

checkType :: TExpr -> PartialResult (TType, TExpr)
checkType (EVar pos ident) = do
  env <- ask
  case (Data.Map.member ident (vars env)) of
    False -> throwError $ tokenPos pos ++ " variable '" ++ (showVarName ident) ++ "' not declared"
    otherwise -> do
      state <- get
      (Just loc) <- return $ Data.Map.lookup ident (vars env)
      (Just vtype) <- return $ Data.Map.lookup loc state
      -- change pos from where var was declared to where it is called
      return (changePos vtype pos, EVar pos ident)

checkType (ELitInt pos num) = do
  case (num > 2147483647) of
    True -> throwError $ tokenPos pos ++ " value larger than MAX_INT"
    otherwise -> do
      case (num < -2147483648) of
        True -> throwError $ tokenPos pos ++ " value lower than MIN_INT"
        otherwise -> return (Int pos, ELitInt pos num)

checkType (ELitTrue pos) = return (Bool pos, ELitTrue pos)
checkType (ELitFalse pos) = return (Bool pos, ELitFalse pos)
checkType (EApp pos ident args) = do
  env <- ask
  case (Data.Map.member ident (vars env)) of
    False -> throwError $ tokenPos pos ++ " function '" ++ (showVarName ident) ++ "' not declared"
    otherwise -> do
      state <- get
      (Just loc) <- return $ Data.Map.lookup ident (vars env)
      (Just (Fun _ retType argTypes)) <- return $ Data.Map.lookup loc state
      args' <- checkParametersPassed args argTypes ident pos
      return (changePos retType pos, EApp pos ident args')

checkType (EString pos str) = return $ (Str pos, EString pos str)

checkType (Neg pos expr) = do
  (actualType, expr') <- checkType expr
  case (actualType == Int (Just (0,0))) of
    True -> do
      case expr' of
        (ELitInt pos2 num) -> return (Int pos, ELitInt pos2 (-num))
        otherwise -> return (Int pos, Neg pos expr')
    otherwise -> throwError " operator '-' can be used only in front of integer expressions"

checkType (Not pos expr) = do
  (actualType, expr') <- checkType expr
  case (actualType == Bool (Just (0,0))) of
    True -> do
      case expr' of
        (ELitTrue pos) -> return (Bool pos, ELitFalse pos)
        (ELitFalse pos) -> return (Bool pos, ELitTrue pos)
        otherwise -> return (Bool pos, Not pos expr')
    otherwise -> throwError " operator '!' can be used only in front of boolean expressions"

checkType (EMul pos expr1 op expr2) = do
  (actualType1, expr1') <- checkType expr1
  (actualType2, expr2') <- checkType expr2
  case (actualType1 == Int (Just (0,0))) of
    True -> do
      checkOperandTypes actualType1 actualType2
      case expr1' of
        (ELitInt pos1 num1) -> do
          case expr2' of
            (ELitInt pos2 num2) -> do
              case op of
                (Times _) -> return (Int pos, ELitInt pos (num1 * num2))
                (Div _) -> do
                  if (num2 == 0) then throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' division by 0"
                                 else return (Int pos, ELitInt pos (num1 `div` num2))
                (Mod _) -> do
                  if (num2 == 0) then throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' division by 0"
                                 else return (Int pos, ELitInt pos (num1 `mod` num2))
            otherwise -> return (Int pos, EMul pos expr1' op expr2')
        otherwise -> return (Int pos, EMul pos expr1' op expr2')
    otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can be performend only on integer operands"

checkType (EAdd pos expr1 op expr2) = do
  (actualType1, expr1') <- checkType expr1
  (actualType2, expr2') <- checkType expr2
  case (actualType1 == Int (Just (0,0))) of
    True -> do
      checkOperandTypes actualType1 actualType2
      case expr1' of
        (ELitInt pos1 num1) -> do
          case expr2' of
            (ELitInt pos2 num2) -> do
              case op of
                (Plus _) -> return (Int pos, ELitInt pos (num1 + num2))
                (Minus _) -> return (Int pos, ELitInt pos (num1 - num2))
            otherwise -> return (Int pos, EAdd pos expr1' op expr2')
        otherwise -> return (Int pos, EAdd pos expr1' op expr2')
    otherwise -> do
      case (actualType1 == Str (Just (0,0)) && op == Plus (Just (0,0))) of
        True -> do
          checkOperandTypes actualType1 actualType2
          return (Str pos, EAdd pos expr1' op expr2')
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"
                                               ++ (show actualType1) ++ "' operand"

checkType (ERel pos expr1 op expr2) = do
  (actualType1, expr1') <- checkType expr1
  (actualType2, expr2') <- checkType expr2
  checkOperandTypes actualType1 actualType2
  case op of
    (EQU _) -> do
      case actualType1 of
        (Int _) -> simplifyIntCmp expr1' expr2' (\n1 n2 -> n1 == n2) pos op
        (Bool _) -> do
          case expr1' of
            (ELitTrue pos1) -> do
              case expr2' of
                (ELitTrue pos2) -> return (Bool pos, ELitTrue pos)
                (ELitFalse pos2) -> return (Bool pos, ELitFalse pos)
                otherwise -> return (Bool pos, ERel pos expr1' op expr2')
            (ELitFalse pos1) -> do
              case expr2' of
                (ELitTrue pos2) -> return (Bool pos, ELitFalse pos)
                (ELitFalse pos2) -> return (Bool pos, ELitTrue pos)
                otherwise -> return (Bool pos, ERel pos expr1' op expr2')
            otherwise -> return (Bool pos, ERel pos expr1' op expr2')
        (Str _) -> do
          checkOperandTypes actualType1 actualType2
          return (Bool pos, ERel pos expr1' op expr2')
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"
                                                      ++ (show actualType1) ++ "' operand"
    (NE _) -> do
      case actualType1 of
        (Int _) -> simplifyIntCmp expr1 expr2 (\n1 n2 -> n1 /= n2) pos op
        (Bool _) -> do
          case expr1' of
            (ELitTrue pos1) -> do
              case expr2' of
                (ELitTrue pos2) -> return (Bool pos, ELitFalse pos)
                (ELitFalse pos2) -> return (Bool pos, ELitTrue pos)
                otherwise -> return (Bool pos, ERel pos expr1' op expr2')
            (ELitFalse pos1) -> do
              case expr2' of
                (ELitTrue pos2) -> return (Bool pos, ELitTrue pos)
                (ELitFalse pos2) -> return (Bool pos, ELitFalse pos)
                otherwise -> return (Bool pos, ERel pos expr1' op expr2')
            otherwise -> return (Bool pos, ERel pos expr1' op expr2')
        (Str _) -> do
          checkOperandTypes actualType1 actualType2
          return (Bool pos, ERel pos expr1' op expr2')
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"
                                                      ++ (show actualType1) ++ "' operand"
    (LTH _) -> do
      case actualType1 of
        (Int _) -> simplifyIntCmp expr1 expr2 (\n1 n2 -> n1 < n2) pos op
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"
    (LE _) -> do
      case actualType1 of
        (Int _) -> simplifyIntCmp expr1 expr2 (\n1 n2 -> n1 <= n2) pos op
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"
    (GTH _) -> do
      case actualType1 of
        (Int _) -> simplifyIntCmp expr1 expr2 (\n1 n2 -> n1 > n2) pos op
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"
    (GE _) -> do
      case actualType1 of
        (Int _) -> simplifyIntCmp expr1 expr2 (\n1 n2 -> n1 >= n2) pos op
        otherwise -> throwError $ tokenPos pos ++ "'" ++ (show op) ++ "' can't be performed on '"

checkType (EAnd pos expr1 expr2) = do
  (actualType1, expr1') <- checkType expr1
  (actualType2, expr2') <- checkType expr2
  checkOperandTypes actualType1 actualType2
  case (actualType1 == Bool (Just (0,0))) of
    True -> do
      case expr1' of
        (ELitFalse pos1) -> return (Bool pos, ELitFalse pos)
        (ELitTrue pos1) -> do
          case expr2' of
            (ELitFalse pos2) -> return (Bool pos, ELitFalse pos)
            (ELitTrue pos2) -> return (Bool pos, ELitTrue pos)
            otherwise -> return (Bool pos, EAnd pos expr1' expr2')
        otherwise -> return (Bool pos, EAnd pos expr1' expr2')
    otherwise -> throwError $ tokenPos pos ++ " logical 'and' can be performend only on boolean operands."

checkType (EOr pos expr1 expr2) = do
  (actualType1, expr1') <- checkType expr1
  (actualType2, expr2') <- checkType expr2
  checkOperandTypes actualType1 actualType2
  case (actualType1 == Bool (Just (0,0))) of
    True -> do
      case expr1' of
        (ELitTrue pos1) -> return (Bool pos, ELitTrue pos)
        (ELitFalse pos1) -> do
          case expr2' of
            (ELitTrue pos2) -> return (Bool pos, ELitTrue pos)
            (ELitFalse pos2) -> return (Bool pos, ELitFalse pos)
            otherwise -> return (Bool pos, EOr pos expr1' expr2')
        otherwise -> return (Bool pos, EOr pos expr1' expr2')
    otherwise -> throwError $ tokenPos pos ++ " logical 'or' can be performend only on boolean operands."

simplifyIntCmp :: TExpr -> TExpr -> (Integer -> Integer -> Bool) -> SPos -> TRelOp -> PartialResult (TType, TExpr)
simplifyIntCmp (ELitInt _ num1) (ELitInt _ num2) rel pos _ = do
  case (rel num1 num2) of
    True -> return (Bool pos, ELitTrue pos)
    False -> return (Bool pos, ELitFalse pos)
simplifyIntCmp expr1 expr2 _ pos op = return (Bool pos, ERel pos expr1 op expr2)

checkOperandTypes :: TType -> TType -> PartialResult ()
checkOperandTypes actualType1 actualType2 = do
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

checkParametersPassed :: [TExpr] -> [TType] -> Ident -> SPos -> PartialResult [TExpr]
checkParametersPassed [] [] _ _ = return []
checkParametersPassed [] _ ident pos = throwError $ tokenPos pos ++ " not enough arguments passed to function '"
                                                    ++ (showVarName ident) ++ "'"
checkParametersPassed _ [] ident pos = throwError $ tokenPos pos ++ " too many arguments passed to function '"
                                                    ++ (showVarName ident) ++ "'"
checkParametersPassed (expr:exprs) (expectedType:argTypes) ident pos = do
  (actualType, expr') <- checkType expr
  case (actualType == expectedType) of
    True -> do
      exprs' <- checkParametersPassed exprs argTypes ident pos
      return (expr':exprs')
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
