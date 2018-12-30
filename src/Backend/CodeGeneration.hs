module Backend.CodeGeneration where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.Map

import Backend.Datatypes
import Frontend.AbsLatte


emitProgram :: TProgram -> Result String
emitProgram (Program _ stmts) = do
  env' <- putFnTypes stmts
  predefinedFns <- local (const env') $ emitPredefinedFnDecls
  functionDefs <- local (const env') $ mapM emitFunction stmts
  state <- get
  return $ unlines $ predefinedFns ++ [""] ++ (globalVarsDefs state) ++ [""] ++ functionDefs

emitPredefinedFnDecls :: Result Instructions
emitPredefinedFnDecls =
  return $ ["declare void @printInt(i32)",
            "declare void @printString(i8*)",
            "declare i32 @readInt()",
            "declare i8* @readString()",
            "declare void @error()"]

emitFunction :: TTopDef -> Result String
emitFunction (FnDef _ rtype (Ident fname) args body) = do
  state <- get
  put $ StateLLVM 0 (globalVarsDefs state) (varsStore state)
  header <- emitHeader rtype fname args
  entry <- emitEntry args
  (status, _, bodyInstrs) <- emitBlock body
  case status of
    Running -> do
      bodyInstrs <- return $ bodyInstrs ++ ["ret void"]
      body <- return $ [if "." `isPrefixOf` instr then instr else "    " ++ instr | instr <- bodyInstrs]
      footer <- return ["}"]
      return $ unlines $ header ++ entry ++ body ++ footer
    Terminated -> do
      body <- return $ [if "." `isPrefixOf` instr then instr else "    " ++ instr | instr <- bodyInstrs]
      footer <- return ["}"]
      return $ unlines $ header ++ entry ++ body ++ footer

emitHeader :: TType -> String -> [TArg] -> Result Instructions
emitHeader rtype fname args =
  return ["define " ++ (llvmType rtype) ++ " @" ++ fname ++ "(" ++ (emitArguments args) ++ ") {"]

emitEntry :: [TArg] -> Result Instructions
-- emitEntry args = return ["entry:"]
emitEntry args = return []

emitBlock :: TBlock -> Result (TStatus, Env, Instructions)
emitBlock (Block _ []) = do
  env <- ask
  return (Running, env, [])
emitBlock (Block pos (stmt:stmts)) = do
  env <- ask
  (status, env', stmtCode) <- emitStmt stmt
  case status of
    Terminated -> return (Terminated, env', stmtCode)
    Running -> do
      (status', _, stmtsCode) <- local (const env') $ emitBlock (Block pos stmts)
      return $ (status', env, stmtCode ++ stmtsCode)

emitStmt :: TStmt -> Result (TStatus, Env, Instructions)
emitStmt (Empty _) = do
  env <- ask
  return (Running, env, [])
emitStmt (BStmt _ block) = do
  env <- ask
  (status, _, instrs) <- emitBlock block
  return (status, env, instrs)
emitStmt (SExp _ expr) = do
  (instructions, _, _) <- emitExpr expr
  env <- ask
  return (Running, env, instructions)
emitStmt (Ret _ expr) = do
  env <- ask
  (instructions, rtype, res) <- emitExpr expr
  return (Terminated, env, instructions ++ ["ret " ++ (llvmType rtype) ++ " " ++ res])
emitStmt (VRet _) = do
  env <- ask
  return (Terminated, env, ["ret void"])
emitStmt (Cond _ bexpr ifTrueStmt) = do
  env <- ask
  case (simplifyConstExpr bexpr) of
    (Just False) -> return (Running, env, [])
    (Just True) -> do
      (status, _, instrs) <- emitStmt ifTrueStmt
      return (status, env, instrs)
    Nothing -> do
      condLabelReg <- getNextRegister
      condLabel <- return $ ".if.cond." ++ (show condLabelReg) ++ ":"
      (condInstrs, _, condReg) <- emitExpr bexpr

      ifTrueLabelReg <- getNextRegister
      ifTrueLabel <- return $ ".if.true." ++ (show ifTrueLabelReg) ++ ":"
      (_, _, ifTrueInstrs) <- emitStmt ifTrueStmt
      afterCondReg <- getNextRegister
      afterCondLabel <- return $ ".after.cond." ++ (show afterCondReg) ++ ":"
      jmpAfterStmt <- return $ ["br label %" ++ (init afterCondLabel)]
      jmpInstr <- return ["br i1 " ++ condReg ++ ", label %" ++ (init ifTrueLabel) ++
                          ", label %" ++ (init afterCondLabel)]
      instrs <- return $ [condLabel] ++ condInstrs ++ jmpInstr ++ [ifTrueLabel] ++ ifTrueInstrs ++ jmpAfterStmt ++
                         [afterCondLabel]
      return (Running, env, instrs)
emitStmt (CondElse _ bexpr ifTrueStmt ifFalseStmt) = do
  env <- ask
  case (simplifyConstExpr bexpr) of
    (Just False) -> do
      (status, _, instrs) <- emitStmt ifFalseStmt
      return (status, env, instrs)
    (Just True) -> do
      (status, _, instrs) <- emitStmt ifTrueStmt
      return (status, env, instrs)
    Nothing -> do
      condLabelReg <- getNextRegister
      condLabel <- return $ ".if.cond." ++ (show condLabelReg) ++ ":"
      (condInstrs, _, condReg) <- emitExpr bexpr

      ifTrueLabelReg <- getNextRegister
      ifTrueLabel <- return $ ".if.true." ++ (show ifTrueLabelReg) ++ ":"
      (ifTrueStatus, _, ifTrueInstrs) <- emitStmt ifTrueStmt
      ifFalseLabelReg <- getNextRegister
      ifFalseLabel <- return $ ".if.false." ++ (show ifFalseLabelReg) ++ ":"
      (ifFalseStatus, _, ifFalseInstrs) <- emitStmt ifFalseStmt

      afterCondReg <- getNextRegister
      afterCondLabel <- return $ ".after.cond." ++ (show afterCondReg) ++ ":"
      jmpAfterStmt <- return $ ["br label %" ++ (init afterCondLabel)]
      jmpInstr <- return ["br i1 " ++ condReg ++ ", label %" ++ (init ifTrueLabel) ++
                          ", label %" ++ (init ifFalseLabel)]
      instrs <- return $ [condLabel] ++ condInstrs ++ jmpInstr ++ [ifTrueLabel] ++ ifTrueInstrs ++ jmpAfterStmt ++
                          [ifFalseLabel] ++ ifFalseInstrs ++ jmpAfterStmt ++ [afterCondLabel]
      if (ifTrueStatus == Terminated && ifFalseStatus == Terminated) then
        return (Terminated, env, instrs)
      else
        return (Running, env, instrs)


emitStmt _ = do
  env <- ask
  return (Running, env, [])


emitExpr :: TExpr -> Result (Instructions, TType, Register)
emitExpr (ELitInt _ num) = return ([], Int (Just (0,0)), show num)
emitExpr (EString _ str) = do
  reg <- getNextRegister
  state <- get
  -- todo add fnName to str def as registers starts from 0 for each fn
  globalVarDef <- return $ "@str" ++ (show reg) ++ " = internal constant ["
                            ++ (show $ (length str) - 1) ++ " x i8] c\"" ++ (tail $ init str) ++ "\\00\""
  put $ StateLLVM (nextRegister state) (globalVarDef : (globalVarsDefs state)) (varsStore state)
  instr <- return $ "%" ++ (show reg) ++ " = getelementptr [" ++ (show $ (length str) - 1) ++ " x i8], " ++
                    "[" ++ (show $ (length str) - 1) ++ " x i8]* @str" ++ (show reg) ++ ", i32 0, i32 0"
  return ([instr], Str (Just (0, 0)), "%" ++ (show reg))
emitExpr (EApp _ (Ident fname) exprs) = do
  state <- get
  exprsRes <- mapM emitExpr exprs
  (instrs, ptypes, pregs) <- return $ unzip3 exprsRes
  rtype <- getVarType (Ident fname)
  fnCall <- return $ "call " ++ (llvmType rtype) ++ " @" ++ fname ++ "(" ++ (emitParameters ptypes pregs) ++ ")"
  case (rtype == (Void (Just (0,0)))) of
    True -> do
      fnCallInstr <- return [[fnCall]]
      instrs' <- return $ instrs ++ fnCallInstr
      return (foldr (++) [] instrs', Int (Just (0, 0)), "%-1")
    False -> do
      register <- getNextRegister
      fnCallInstr <- return $ [["%" ++ (show register) ++ " = " ++ fnCall]]
      instrs' <- return $ instrs ++ fnCallInstr
      return (foldr (++) [] instrs', Int (Just (0, 0)), "%" ++ (show register))


emitExpr _ = return ([], Int (Just (0, 0)), "0")

emitArguments :: [TArg] -> String
emitArguments args = intercalate ", " (emitArguments' args) where
  emitArguments' [] = []
  emitArguments' ((Arg _ atype (Ident aname)):args) = ((llvmType atype) ++ " %" ++ aname) : (emitArguments' args)

emitParameters :: [TType] -> [String] -> String
emitParameters ptypes pregs = intercalate ", " (emitParameters' ptypes pregs) where
  emitParameters' [] [] = []
  emitParameters' (ptype:ptypes) (preg:pregs) = ((llvmType ptype) ++ " " ++ preg) : (emitParameters' ptypes pregs)

getNextRegister :: Result Integer
getNextRegister = do
  state <- get
  nextReg <- return $ nextRegister state
  put $ StateLLVM (nextReg + 1) (globalVarsDefs state) (varsStore state)
  return nextReg

getVarType:: Ident -> Result TType
getVarType vname = do
  env <- ask
  maybeLoc <- return $ Data.Map.lookup vname env
  case maybeLoc of
    (Just loc) -> do
      state <- get
      case (Data.Map.lookup loc (varsStore state)) of
        (Just (rtype, _)) -> return rtype
        Nothing -> throwError "Value under loc was not found in store"
    Nothing -> throwError "Loc not found for variable"

putFnTypes :: [TTopDef] -> Result Env
putFnTypes [] = do
  env' <- putFn (Ident "printInt") (Void (Just (0, 0)))
  env'' <- local (const env') $ putFn (Ident "printString") (Void (Just (0, 0)))
  env''' <- local (const env'') $ putFn (Ident "error") (Void (Just (0, 0)))
  env'''' <- local (const env''') $ putFn (Ident "readInt") (Int (Just (0, 0)))
  env''''' <- local (const env'''') $ putFn (Ident "readString") (Str (Just (0, 0)))
  return env'''''

putFnTypes ((FnDef _ rtype fname _ _):stmts) = do
  env' <- putFn fname rtype
  local (const env') $ putFnTypes stmts

putFn :: Ident -> TType -> Result Env
putFn fname fnRtype = do
  env <- ask
  loc <- newloc
  env' <- return $ Data.Map.insert fname loc env
  state <- get
  newstore <- return $ Data.Map.insert loc (fnRtype, "$$$") (varsStore state) -- todo $$$
  put $ StateLLVM (nextRegister state) (globalVarsDefs state) newstore
  return env'

newloc :: Result Loc
newloc = do
  state <- get
  return $ Data.Map.size (varsStore state)

llvmType :: TType -> String
llvmType (Int _) = "i32"
llvmType (Str _) = "i8*"
llvmType (Bool _) = "i1"
llvmType (Void _) = "void"
-- llvmType _ = "**err**"

simplifyConstExpr :: TExpr -> Maybe Bool
simplifyConstExpr (ELitTrue _) = Just True
simplifyConstExpr (ELitFalse _) = Just False
simplifyConstExpr _ = Nothing