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
  put $ StateLLVM 0 0 0 fname (globalVarsDefs state) (varsStore state)
  header <- emitHeader rtype fname args
  entry <- emitEntry args
  (status, _, bodyInstrs) <- emitBlock body
  case status of
    Running -> do
      bodyInstrs <- return $ bodyInstrs ++ ["ret void"]
      body <- return $ [if "." `isPrefixOf` instr then instr else "    " ++ instr | instr <- bodyInstrs]
      footer <- return ["}"]
      return $ unlines $ header ++ entry ++ body ++ footer
    otherwise -> do
      body <- return $ [if "." `isPrefixOf` instr then instr else "    " ++ instr | instr <- bodyInstrs]
      footer <- return ["}"]
      return $ unlines $ header ++ entry ++ body ++ footer

emitHeader :: TType -> String -> [TArg] -> Result Instructions
emitHeader rtype fname args =
  return ["define " ++ (llvmType rtype) ++ " @" ++ fname ++ "(" ++ (emitArguments args) ++ ") {"]

emitEntry :: [TArg] -> Result Instructions
emitEntry args = do
  entryLabelNum <- getNextLabelNum
  return [".entry." ++ (show entryLabelNum) ++ ":"]

emitBlock :: TBlock -> Result (TStatus, Env, Instructions)
emitBlock (Block _ []) = do
  env <- ask
  return (Running, env, [])
emitBlock (Block pos (stmt:stmts)) = do
  env <- ask
  (status, env', stmtCode) <- emitStmt stmt
  case status of
    Terminated -> return (Terminated, env', stmtCode)
    otherwise -> do
      (status', _, stmtsCode) <- local (const env') $ emitBlock (Block pos stmts)
      return $ (status', env, stmtCode ++ stmtsCode)

emitStmt :: TStmt -> Result (TStatus, Env, Instructions)
emitStmt (Empty _) = do
  env <- ask
  return (Running, env, [])
emitStmt (BStmt _ block) = do
  env <- ask
  incrementBlockNum
  (status, _, instrs) <- emitBlock block
  return (status, env, instrs)
emitStmt (Decl _ _ []) = do
  env <- ask
  return (Running, env, [])
emitStmt (Decl pos vtype ((NoInit _ (Ident vname)):vitems)) = do
  env <- ask
  loc <- newloc
  env' <- return $ Data.Map.insert (Ident vname) loc env
  state <- get
  newstore <- return $ Data.Map.insert loc (vtype, vname) (varsStore state)
  put $ StateLLVM (nextRegister state) (nextLabel state) (nextBlock state) (fnName state) (globalVarsDefs state) newstore

  blockNum <- getCurrentBlockNum
  declVarInstrs <- return ["%" ++ vname ++ "." ++ (show blockNum) ++ " = alloca " ++ (llvmType vtype)]

  (_, env'', declVarsInstrs) <- local (const env') $ emitStmt (Decl pos vtype vitems)
  return (Running, env'', declVarInstrs ++ declVarsInstrs)
emitStmt (Decl pos vtype ((Init _ (Ident vname) expr):vitems)) = do
  env <- ask
  loc <- newloc
  env' <- return $ Data.Map.insert (Ident vname) loc env
  state <- get
  newstore <- return $ Data.Map.insert loc (vtype, vname) (varsStore state)
  put $ StateLLVM (nextRegister state) (nextLabel state) (nextBlock state) (fnName state) (globalVarsDefs state) newstore

  blockNum <- getCurrentBlockNum
  declVarInstrs <- return ["%" ++ vname ++ "." ++ (show blockNum) ++ " = alloca " ++ (llvmType vtype)]
  (_, _, initInstrs) <- emitStmt (Ass pos (Ident vname) expr)
  (_, env'', declVarsInstrs) <- local (const env') $ emitStmt (Decl pos vtype vitems)
  return (Running, env'', declVarInstrs ++ initInstrs ++ declVarsInstrs)
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
emitStmt (Cond _ cond ifTrueStmt) = do
  env <- ask
  case (simplifyConstExpr cond) of
    (Just False) -> return (Running, env, [])
    (Just True) -> do
      (status, _, instrs) <- emitStmt ifTrueStmt
      return (status, env, instrs)
    Nothing -> do
      condLabelNum <- getNextLabelNum
      condLabel <- return $ ".if.cond." ++ (show condLabelNum) ++ ":"
      (condInstrs, _, condReg) <- emitExpr cond

      ifTrueLabelNum <- getNextLabelNum
      ifTrueLabel <- return $ ".if.true." ++ (show ifTrueLabelNum) ++ ":"
      (status, _, ifTrueInstrs) <- emitStmt ifTrueStmt
      afterCondLabelNum <- getNextLabelNum
      afterCondLabel <- return $ ".after.cond." ++ (show afterCondLabelNum) ++ ":"
      jmpAfterStmt <- return $ ["br label %" ++ (init afterCondLabel)]
      jmpInstr <- return ["br i1 " ++ condReg ++ ", label %" ++ (init ifTrueLabel) ++
                          ", label %" ++ (init afterCondLabel)]
      jmpToCond <- return ["br label %" ++ (init condLabel)]
      instrs <- return $ jmpToCond ++ [condLabel] ++ condInstrs ++ jmpInstr ++ [ifTrueLabel] ++ ifTrueInstrs ++
                         jmpAfterStmt ++ [afterCondLabel]
      if (status == Running) then return (Running, env, instrs) else return (MaybeTerminated, env, instrs)
emitStmt (CondElse _ cond ifTrueStmt ifFalseStmt) = do
  env <- ask
  case (simplifyConstExpr cond) of
    (Just False) -> do
      (status, _, instrs) <- emitStmt ifFalseStmt
      return (status, env, instrs)
    (Just True) -> do
      (status, _, instrs) <- emitStmt ifTrueStmt
      return (status, env, instrs)
    Nothing -> do
      condLabelNum <- getNextLabelNum
      condLabel <- return $ ".if.cond." ++ (show condLabelNum) ++ ":"
      (condInstrs, _, condReg) <- emitExpr cond

      ifTrueLabelNum <- getNextLabelNum
      ifTrueLabel <- return $ ".if.true." ++ (show ifTrueLabelNum) ++ ":"
      (ifTrueStatus, _, ifTrueInstrs) <- emitStmt ifTrueStmt

      ifFalseLabelNum <- getNextLabelNum
      ifFalseLabel <- return $ ".if.false." ++ (show ifFalseLabelNum) ++ ":"
      (ifFalseStatus, _, ifFalseInstrs) <- emitStmt ifFalseStmt

      afterCondLabelNum <- getNextLabelNum
      afterCondLabel <- return $ ".after.cond." ++ (show afterCondLabelNum) ++ ":"

      jmpToCond <- return ["br label %" ++ (init condLabel)]
      jmpAfterStmt <- return $ ["br label %" ++ (init afterCondLabel)]

      ifTrueInstrs' <- return $ appendJmpInstrs ifTrueStatus ifTrueInstrs jmpAfterStmt
      ifFalseInstrs' <- return $ appendJmpInstrs ifFalseStatus ifFalseInstrs jmpAfterStmt

      jmpInstr <- return ["br i1 " ++ condReg ++ ", label %" ++ (init ifTrueLabel) ++
                          ", label %" ++ (init ifFalseLabel)]

      if (ifTrueStatus == Terminated && ifFalseStatus == Terminated) then do
        instrs <- return $ jmpToCond ++ [condLabel] ++ condInstrs ++ jmpInstr ++ [ifTrueLabel] ++ ifTrueInstrs' ++
                           [ifFalseLabel] ++ ifFalseInstrs
        return (Terminated, env, instrs)
      else do
        instrs <- return $ jmpToCond ++ [condLabel] ++ condInstrs ++ jmpInstr ++ [ifTrueLabel] ++ ifTrueInstrs' ++
                                 [ifFalseLabel] ++ ifFalseInstrs' ++ [afterCondLabel]
        if (ifTrueStatus == Running && ifFalseStatus == Running) then
          return (Running, env, instrs)
        else
          return (MaybeTerminated, env, instrs)
emitStmt (While _ cond body) = do
  env <- ask
  case (simplifyConstExpr cond) of
    (Just False) -> return (Running, env, [])
    (Just True) -> do
      loopBodyLabelNum <- getNextLabelNum
      loopBodyLabel <- return $ ".loop.body." ++ (show loopBodyLabelNum) ++ ":"
      (_, _, loopBodyInstrs) <- emitStmt body
      jmpInstr <- return $ ["br label %" ++ (init loopBodyLabel)]
      instrs <- return $ [loopBodyLabel] ++ loopBodyInstrs ++ jmpInstr
      return (Terminated, env, instrs)
    Nothing -> do
      loopBodyLabelNum <- getNextLabelNum
      loopBodyLabel <- return $ ".loop.body." ++ (show loopBodyLabelNum) ++ ":"
      (_, _, loopBodyInstrs) <- emitStmt body

      loopCondLabelNum <- getNextLabelNum
      loopCondLabel <- return $ ".loop.cond." ++ (show loopCondLabelNum) ++ ":"
      (condInstrs, _, condReg) <- emitExpr cond

      afterLoopLabelNum <- getNextLabelNum
      afterLoopLabel <- return $ ".after.loop." ++ (show afterLoopLabelNum) ++ ":"
      jmpAfterLoop <- return ["br i1 " ++ condReg ++ ", label %" ++ (init loopBodyLabel) ++
                              ", label %" ++ (init afterLoopLabel)]
      jmpToCond <- return $ ["br label %" ++ (init loopCondLabel)]
      jmpToBody <- return $ ["br label %" ++ (init loopBodyLabel)]

      -- is jmp before label necessary / expected?
      instrs <- return $ jmpToCond ++ [loopBodyLabel] ++ loopBodyInstrs ++ jmpToCond ++ [loopCondLabel] ++ condInstrs ++
                         jmpAfterLoop ++ [afterLoopLabel]
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
  put $ StateLLVM (nextRegister state) (nextLabel state) (nextBlock state) (fnName state)
                  (globalVarDef : (globalVarsDefs state)) (varsStore state)
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

appendJmpInstrs :: TStatus -> Instructions -> Instructions -> Instructions
appendJmpInstrs Terminated instrs jmpInstr = instrs
appendJmpInstrs _ instrs jmpInstr = instrs ++ jmpInstr

getCurrentRegister :: Result Integer
getCurrentRegister = do
  state <- get
  return $ nextRegister state

getNextRegister :: Result Integer
getNextRegister = do
  state <- get
  newReg <- return $ nextRegister state
  put $ StateLLVM (newReg + 1) (nextLabel state) (nextBlock state) (fnName state) (globalVarsDefs state) (varsStore state)
  return newReg

getNextLabelNum :: Result Integer
getNextLabelNum = do
  state <- get
  newLabel <- return $ nextLabel state
  put $ StateLLVM (nextRegister state) (newLabel + 1) (nextBlock state) (fnName state) (globalVarsDefs state) (varsStore state)
  return newLabel

getCurrentBlockNum :: Result Integer
getCurrentBlockNum = do
  state <- get
  return $ nextBlock state

incrementBlockNum :: Result ()
incrementBlockNum = do
  state <- get
  put $ StateLLVM (nextRegister state) (nextLabel state) ((nextBlock state) + 1) (fnName state) (globalVarsDefs state)
                  (varsStore state)

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
  newstore <- return $ Data.Map.insert loc (fnRtype, "@" ++ (fnName state)) (varsStore state) -- todo $$$
  put $ StateLLVM (nextRegister state) (nextLabel state) (nextBlock state) (fnName state) (globalVarsDefs state) newstore
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