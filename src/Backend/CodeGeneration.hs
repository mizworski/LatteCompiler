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
  return $ unlines $ predefinedFns ++ [""] ++ (reverse $ globalVarsDefs state) ++ [""] ++ functionDefs

emitPredefinedFnDecls :: Result Instructions
emitPredefinedFnDecls =
  return $ ["declare void @printInt(i32)",
            "declare void @printString(i8*)",
            "declare i32 @readInt()",
            "declare i8* @readString()",
            "declare void @error()",
            "declare i8* @__concat(i8*, i8*)"]

emitFunction :: TTopDef -> Result String
emitFunction (FnDef _ rtype (Ident fname) args body) = do
  state <- get
  put $ StateLLVM 0 0 0 fname (globalVarsDefs state) (varsStore state) ""
  (env, header) <- emitHeader rtype fname args
  (status, _, bodyInstrs) <- local (const env) $ emitBlock body
  case status of
    Running -> do
      bodyInstrs <- return $ bodyInstrs ++ ["ret void"]
      body <- return $ [if "." `isPrefixOf` instr then instr else "    " ++ instr | instr <- bodyInstrs]
      footer <- return ["}"]
      return $ unlines $ header ++ body ++ footer
    otherwise -> do
      body <- return $ [if "." `isPrefixOf` instr then instr else "    " ++ instr | instr <- bodyInstrs]
      footer <- return ["}"]
      return $ unlines $ header ++ body ++ footer

emitHeader :: TType -> String -> [TArg] -> Result (Env, Instructions)
emitHeader rtype fname args = do
  (env, declInstrs, argsInstr) <- emitArguments args
  entry <- emitEntry args
  instrs <- return $ ["define " ++ (llvmType rtype) ++ " @" ++ fname ++ "(" ++ argsInstr ++ ") {"] ++ entry ++ declInstrs
  return (env, instrs)

emitEntry :: [TArg] -> Result Instructions
emitEntry args = do
  entryLabel <- updateCurrentLabel ".entry"
  return [entryLabel ++ ":"]

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
  incrementBlockNum
  return (status, env, instrs)
emitStmt (Decl _ _ []) = do
  env <- ask
  return (Running, env, [])
emitStmt (Decl pos vtype ((NoInit pos2 vident):vitems)) = do
  emitStmt (Decl pos vtype ((Init pos2 vident (getDefaultValExpr vtype)):vitems))
emitStmt (Decl pos vtype ((Init _ vident expr):vitems)) = do
  (initInstrs, _, resReg) <- emitExpr expr
--   (vtype, vreg) <- getVar vident
  (env', varReg) <- declareVarInternally vident vtype
  declVarInstrs <- return [varReg ++ " = alloca " ++ (llvmType vtype)]
  assignInstr <- return ["store " ++ (llvmType vtype) ++ " " ++ resReg ++ ", " ++ (llvmType vtype) ++ "* " ++ varReg]
--   (_, _, initInstrs) <- local (const env') $ emitStmt (Ass pos vident expr)
  (_, env'', declVarsInstrs) <- local (const env') $ emitStmt (Decl pos vtype vitems)
  return (Running, env'', declVarInstrs ++ initInstrs ++ assignInstr ++ declVarsInstrs)
emitStmt (Ass _ vident expr) = do
  env <- ask
  (instrs, _, resReg) <- emitExpr expr
  (vtype, vreg) <- getVar vident
  assignInstr <- return ["store " ++ (llvmType vtype) ++ " " ++ resReg ++ ", " ++ (llvmType vtype) ++ "* " ++ vreg]
  return (Running, env, instrs ++ assignInstr)
emitStmt (Incr pos vident) = do
  addOneExpr <- return (EAdd pos (EVar pos vident) (Plus pos) (ELitInt pos 1))
  emitStmt (Ass pos vident addOneExpr)
emitStmt (Decr pos vident) = do
  addOneExpr <- return (EAdd pos (EVar pos vident) (Minus pos) (ELitInt pos 1))
  emitStmt (Ass pos vident addOneExpr)

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
      condLabel <- updateCurrentLabel ".if.cond"
      (condInstrs, _, condReg) <- emitExpr cond

      ifTrueLabel <- updateCurrentLabel ".if.true"
      (ifTrueStatus, _, ifTrueInstrs) <- emitStmt ifTrueStmt

      afterCondLabel <- updateCurrentLabel ".after.cond"

      jmpAfterStmt <- return $ ["br label %" ++ afterCondLabel]
      jmpInstr <- return ["br i1 " ++ condReg ++ ", label %" ++ ifTrueLabel ++
                          ", label %" ++ afterCondLabel]
      jmpToCond <- return ["br label %" ++ condLabel]
      ifTrueInstrs' <- appendJmpIfNotTerminated ifTrueStatus ifTrueInstrs jmpAfterStmt

      instrs <- return $ jmpToCond ++ [condLabel ++ ":"] ++ condInstrs ++
                         jmpInstr ++ [ifTrueLabel ++ ":"] ++ ifTrueInstrs' ++ [afterCondLabel ++ ":"]
      if (ifTrueStatus == Running) then return (Running, env, instrs) else return (MaybeTerminated, env, instrs)
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
      condLabel <- updateCurrentLabel ".if.cond"
      (condInstrs, _, condReg) <- emitExpr cond

      ifTrueLabel <- updateCurrentLabel ".if.true"
      (ifTrueStatus, _, ifTrueInstrs) <- emitStmt ifTrueStmt

      ifFalseLabel <- updateCurrentLabel ".if.false"
      (ifFalseStatus, _, ifFalseInstrs) <- emitStmt ifFalseStmt

      afterCondLabel <- updateCurrentLabel ".after.cond"

      jmpToCond <- return ["br label %" ++ condLabel]
      jmpAfterStmt <- return $ ["br label %" ++ afterCondLabel]

      -- todo merge instrs with jmp (dont jump if status == terminated)
      ifTrueInstrs' <- appendJmpIfNotTerminated ifTrueStatus ifTrueInstrs jmpAfterStmt
      ifFalseInstrs' <- appendJmpIfNotTerminated ifFalseStatus ifFalseInstrs jmpAfterStmt

      jmpInstr <- return ["br i1 " ++ condReg ++ ", label %" ++ ifTrueLabel ++
                          ", label %" ++ ifFalseLabel]

      if (ifTrueStatus == Terminated && ifFalseStatus == Terminated) then do
        instrs <- return $ jmpToCond ++ [condLabel ++ ":"] ++ condInstrs ++ jmpInstr ++
                           [ifTrueLabel ++ ":"] ++ ifTrueInstrs' ++
                           [ifFalseLabel ++ ":"] ++ ifFalseInstrs
        return (Terminated, env, instrs)
      else do
        instrs <- return $ jmpToCond ++ [condLabel ++ ":"] ++ condInstrs ++ jmpInstr ++
                           [ifTrueLabel ++ ":"] ++ ifTrueInstrs' ++
                           [ifFalseLabel ++ ":"] ++ ifFalseInstrs' ++ [afterCondLabel ++ ":"]
        if (ifTrueStatus == Running && ifFalseStatus == Running) then
          return (Running, env, instrs)
        else
          return (MaybeTerminated, env, instrs)
emitStmt (While _ cond body) = do
  env <- ask
  case (simplifyConstExpr cond) of
    (Just False) -> return (Running, env, [])
    (Just True) -> do
      loopBodyLabel <- updateCurrentLabel ".loop.body"
      (loopBodyStatus, _, loopBodyInstrs) <- emitStmt body
      jmpInstr <- return $ ["br label %" ++ loopBodyLabel]
      loopBodyInstrs' <- appendJmpIfNotTerminated loopBodyStatus loopBodyInstrs jmpInstr
      instrs <- return $ [loopBodyLabel ++ ":"] ++ loopBodyInstrs'
      return (Terminated, env, instrs)
    Nothing -> do
      loopBodyLabel <- updateCurrentLabel ".loop.body"
      (loopBodyStatus, _, loopBodyInstrs) <- emitStmt body

      loopCondLabel <- updateCurrentLabel ".loop.cond"
      (condInstrs, _, condReg) <- emitExpr cond

      afterLoopLabel <- updateCurrentLabel ".after.loop"
      jmpAfterLoop <- return ["br i1 " ++ condReg ++ ", label %" ++ loopBodyLabel ++
                              ", label %" ++ afterLoopLabel]
      jmpToCond <- return $ ["br label %" ++ loopCondLabel]
      jmpToBody <- return $ ["br label %" ++ init loopBodyLabel]

      loopBodyInstrs' <- appendJmpIfNotTerminated loopBodyStatus loopBodyInstrs jmpToCond

      instrs <- return $ jmpToCond ++ [loopBodyLabel ++ ":"] ++ loopBodyInstrs' ++
                         [loopCondLabel ++ ":"] ++ condInstrs ++
                         jmpAfterLoop ++ [afterLoopLabel ++ ":"]
      return (Running, env, instrs)

emitExpr :: TExpr -> Result (Instructions, TType, Register)
emitExpr (EVar pos vident) = do
  (vtype, vreg) <- getVar vident
  register <- getNextRegister
  loadInstrs <- return ["%" ++ (show register) ++ " = load " ++ (llvmType vtype) ++ ", " ++ (llvmType vtype) ++ "* " ++ vreg]
  return (loadInstrs, vtype, "%" ++ (show register))
emitExpr (ELitInt _ num) = return ([], Int (Just (0,0)), show num)
emitExpr (ELitTrue _) = return ([], Bool (Just (0,0)), "1")
emitExpr (ELitFalse _) = return ([], Bool (Just (0,0)), "0")
emitExpr (EString _ str) = do
  reg <- getNextRegister
  state <- get
  strReg <- return $ "@.str." ++ (fnName state) ++ "." ++ (show reg)
  -- todo dlaczego stringi maja zawsze wokolo '"' (np. str == "xx")?
  globalVarDef <- return $  strReg ++ " = internal constant ["
                            ++ (show $ (length str) - 1) ++ " x i8] c\"" ++ (tail $ init str) ++ "\\00\""
  put $ StateLLVM (nextRegister state) (nextLabel state) (nextBlock state) (fnName state)
                  (globalVarDef : (globalVarsDefs state)) (varsStore state) (currentLabel state)
  instr <- return $ "%" ++ (show reg) ++ " = getelementptr [" ++ (show $ (length str) - 1) ++ " x i8], " ++
                    "[" ++ (show $ (length str) - 1) ++ " x i8]* " ++ strReg ++ ", i32 0, i32 0"
  return ([instr], Str (Just (0, 0)), "%" ++ (show reg))
emitExpr (EApp _ (Ident fname) exprs) = do
  state <- get
  exprsRes <- mapM emitExpr exprs
  (instrs, ptypes, pregs) <- return $ unzip3 exprsRes
  (rtype, _) <- getVar (Ident fname)
  fnCall <- return $ "call " ++ (llvmType rtype) ++ " @" ++ fname ++ "(" ++ (emitParameters ptypes pregs) ++ ")"
  case (rtype == (Void (Just (0,0)))) of
    True -> do
      fnCallInstr <- return [[fnCall]]
      instrs' <- return $ instrs ++ fnCallInstr
      return (foldr (++) [] instrs', rtype, "%(-1)")
    False -> do
      register <- getNextRegister
      fnCallInstr <- return $ [["%" ++ (show register) ++ " = " ++ fnCall]]
      instrs' <- return $ instrs ++ fnCallInstr
      return (foldr (++) [] instrs', rtype, "%" ++ (show register))
emitExpr (Neg pos expr) = emitExpr (EAdd pos (ELitInt pos 0) (Minus pos) expr)
emitExpr (Not pos bexpr) = do
  (instrs, _, resReg) <- emitExpr bexpr
  register <- getNextRegister
  negateInstrs <- return ["%" ++ (show register) ++ " = sub i1 1, " ++ resReg]
  return (instrs ++ negateInstrs, Bool (Just (0, 0)), "%" ++ (show register))
emitExpr (EMul _ expr1 (Mod _) expr2) = do
  (instrs1, etype, reg1) <- emitExpr expr1
  (instrs2, _, reg2) <- emitExpr expr2
  reg <- getNextRegister
  -- todo change remainder to modulo
  mulInstr <- return ["%" ++ (show reg) ++ " = srem " ++ (llvmType etype) ++ " " ++ reg1 ++ ", " ++ reg2]
  return (instrs1 ++ instrs2 ++ mulInstr, etype, "%" ++ (show reg))
emitExpr (EMul _ expr1 op expr2) = do
  (instrs1, etype, reg1) <- emitExpr expr1
  (instrs2, _, reg2) <- emitExpr expr2
  reg <- getNextRegister
  mulInstr <- return ["%" ++ (show reg) ++ " = " ++ (getMulOpInstr op) ++ " " ++ (llvmType etype) ++ " " ++ reg1 ++ ", " ++ reg2]
  return (instrs1 ++ instrs2 ++ mulInstr, etype, "%" ++ (show reg))
emitExpr (EAdd _ expr1 op expr2) = do
  (instrs1, etype, reg1) <- emitExpr expr1
  (instrs2, _, reg2) <- emitExpr expr2
  reg <- getNextRegister
  case (etype == (Str (Just (0, 0)))) of
    True -> do
      addInstr <- return ["%" ++ (show reg) ++ " = call i8* @__concat(i8* " ++ reg1 ++ ", i8* " ++ reg2 ++ ")"]
      return (instrs1 ++ instrs2 ++ addInstr, etype, "%" ++ (show reg))
    False -> do
      addInstr <- return ["%" ++ (show reg) ++ " = " ++ (getAddOpInstr op) ++ " " ++ (llvmType etype) ++ " " ++ reg1 ++ ", " ++ reg2]
      return (instrs1 ++ instrs2 ++ addInstr, etype, "%" ++ (show reg))
emitExpr (ERel _ expr1 op expr2) = do
  (instrs1, etype, reg1) <- emitExpr expr1
  (instrs2, _, reg2) <- emitExpr expr2
  reg <- getNextRegister
  cmpInstr <- return ["%" ++ (show reg) ++ " = icmp " ++ (showRelOp op) ++ " " ++ (llvmType etype) ++ " " ++ reg1 ++ ", " ++ reg2]
  return (instrs1 ++ instrs2 ++ cmpInstr, Bool (Just (0, 0)), "%" ++ (show reg))
emitExpr (EAnd _ expr1 expr2) = do
  leftExprLabel <- updateCurrentLabel ".land.left.expr"
  (instrs1, etype, reg1) <- emitExpr expr1
  regCond <- getNextRegister
  condJmpRightExpr <- return ["%" ++ (show regCond) ++ " = icmp eq i1 " ++ reg1 ++ ", 1"]
  labelAfterLeftExprEmit <- getCurrentLabel

  rightExprLabel <- updateCurrentLabel ".land.right.expr"
  (instrs2, _, reg2) <- emitExpr expr2
  labelAfterRightExprEmit <- getCurrentLabel

  landAfterLabel <- updateCurrentLabel ".land.after"
  jmpRightExpr <- return $ ["br i1 %" ++ (show regCond) ++ ", label %" ++ rightExprLabel ++ ", label %" ++ landAfterLabel]
  jmpLeftExpr <- return ["br label %" ++ leftExprLabel]
  jmpDone <- return ["br label %" ++ landAfterLabel]


  regRes <- getNextRegister
  resInstr <- return ["%" ++ (show regRes) ++ " = phi i1 [ 0, %" ++ labelAfterLeftExprEmit ++ " ], " ++
                      "[ " ++ reg2 ++ ", %" ++ labelAfterRightExprEmit ++ " ]"]

  instrs <- return $ jmpLeftExpr ++ [leftExprLabel ++ ":"] ++ instrs1 ++ condJmpRightExpr ++
                     jmpRightExpr ++ [rightExprLabel ++ ":"] ++ instrs2 ++
                     jmpDone ++ [landAfterLabel ++ ":"] ++ resInstr
  return (instrs, etype, "%" ++ (show regRes))
emitExpr (EOr _ expr1 expr2) = do
  leftExprLabel <- updateCurrentLabel ".lor.left.expr"
  (instrs1, etype, reg1) <- emitExpr expr1
  regCond <- getNextRegister
  condJmpRightExpr <- return ["%" ++ (show regCond) ++ " = icmp eq i1 " ++ reg1 ++ ", 1"]
  labelAfterLeftExprEmit <- getCurrentLabel


  rightExprLabel <- updateCurrentLabel ".lor.right.expr"
  (instrs2, _, reg2) <- emitExpr expr2
  labelAfterRightExprEmit <- getCurrentLabel

  lorAfterLabel <- updateCurrentLabel ".lor.after"
  jmpRightExpr <- return $ ["br i1 %" ++ (show regCond) ++ ", label %" ++ lorAfterLabel ++ ", label %" ++ rightExprLabel]
  jmpLeftExpr <- return ["br label %" ++ leftExprLabel]
  jmpDone <- return ["br label %" ++ lorAfterLabel]

  regRes <- getNextRegister
  resInstr <- return ["%" ++ (show regRes) ++ " = phi i1 [ 1, %" ++ labelAfterLeftExprEmit ++ " ], " ++
                      "[ " ++ reg2 ++ ", %" ++ labelAfterRightExprEmit ++ " ]"]

  instrs <- return $ jmpLeftExpr ++ [leftExprLabel ++ ":"] ++ instrs1 ++ condJmpRightExpr ++
                     jmpRightExpr ++ [rightExprLabel ++ ":"] ++ instrs2 ++
                     jmpDone ++ [lorAfterLabel ++ ":"] ++ resInstr
  return (instrs, etype, "%" ++ (show regRes))

emitArguments :: [TArg] -> Result (Env, Instructions, String)
emitArguments args = do
  (env, declInstrs, llvmArgStrs) <- emitArguments' args [] []
  llvmArgsStr <- return $ intercalate ", " llvmArgStrs
  return (env, declInstrs, llvmArgsStr)
  where
  emitArguments' [] accDecls accSignature = do
    env <- ask
    return (env, reverse accDecls, reverse accSignature)
  emitArguments' ((Arg pos atype (Ident aname)):args) accDecls accSignature = do
--     initExpr <- return $ (EVar )
--     (_, env, declInstr) <- emitStmt (Decl pos atype ((Init pos (Ident aname) initExpr):[]))
    (env, argReg) <- declareVarInternally (Ident aname) atype
    declInstr <- return $ argReg ++ " = alloca " ++ (llvmType atype)
    storeInstr <- return $ "store " ++ (llvmType atype) ++ " %" ++ aname ++ ", " ++ (llvmType atype) ++ "* " ++ argReg
    declInstrs <- return $ [storeInstr] ++ [declInstr] --first store then decl, because it's reversed after in the end
    argStr <- return $ (llvmType atype) ++ " %" ++ aname
    local (const env) $ emitArguments' args (declInstrs ++ accDecls) (argStr : accSignature)

emitParameters :: [TType] -> [String] -> String
emitParameters ptypes pregs = intercalate ", " (emitParameters' ptypes pregs) where
  emitParameters' [] [] = []
  emitParameters' (ptype:ptypes) (preg:pregs) = ((llvmType ptype) ++ " " ++ preg) : (emitParameters' ptypes pregs)

getCurrentRegister :: Result Integer
getCurrentRegister = do
  state <- get
  return $ nextRegister state

getNextRegister :: Result Integer
getNextRegister = do
  state <- get
  newReg <- return $ nextRegister state
  put $ StateLLVM (newReg + 1) (nextLabel state) (nextBlock state) (fnName state) (globalVarsDefs state)
                  (varsStore state) (currentLabel state)
  return newReg

getCurrentBlockNum :: Result Integer
getCurrentBlockNum = do
  state <- get
  return $ nextBlock state

incrementBlockNum :: Result ()
incrementBlockNum = do
  state <- get
  put $ StateLLVM (nextRegister state) (nextLabel state) ((nextBlock state) + 1) (fnName state) (globalVarsDefs state)
                  (varsStore state) (currentLabel state)

putFnTypes :: [TTopDef] -> Result Env
putFnTypes [] = do
  env' <- putFn (Ident "printInt") (Void (Just (0, 0)))
  env'' <- local (const env') $ putFn (Ident "printString") (Void (Just (0, 0)))
  env''' <- local (const env'') $ putFn (Ident "error") (Void (Just (0, 0)))
  env4 <- local (const env''') $ putFn (Ident "readInt") (Int (Just (0, 0)))
  env5 <- local (const env4) $ putFn (Ident "readString") (Str (Just (0, 0)))
  env6 <- local (const env5) $ putFn (Ident "__concat") (Str (Just (0, 0)))
  return env6

putFnTypes ((FnDef _ rtype fname _ _):stmts) = do
  env' <- putFn fname rtype
  local (const env') $ putFnTypes stmts

putFn :: Ident -> TType -> Result Env
putFn fname fnRtype = do
  env <- ask
  loc <- newloc
  env' <- return $ Data.Map.insert fname loc env
  state <- get
  newstore <- return $ Data.Map.insert loc (fnRtype, "@" ++ (fnName state)) (varsStore state)
  put $ StateLLVM (nextRegister state) (nextLabel state) (nextBlock state) (fnName state) (globalVarsDefs state)
                  newstore (currentLabel state)
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

getMulOpInstr :: TMulOp -> String
getMulOpInstr (Times _) = "mul"
getMulOpInstr (Div _) = "sdiv"

getAddOpInstr :: TAddOp -> String
getAddOpInstr (Plus _) = "add"
getAddOpInstr (Minus _) = "sub"

showRelOp :: TRelOp -> String
showRelOp (LTH _) = "slt"
showRelOp (LE _) = "sle"
showRelOp (GTH _) = "sgt"
showRelOp (GE _) = "sge"
showRelOp (EQU _) = "eq"
showRelOp (NE _) = "ne"

declareVarInternally :: Ident -> TType -> Result (Env, String)
declareVarInternally (Ident vname) vtype = do
    env <- ask
    loc <- newloc
    env' <- return $ Data.Map.insert (Ident vname) loc env
    state <- get
    blockNum <- getCurrentBlockNum
    varReg <- return $ "%" ++ vname ++ "." ++ (show blockNum)
    newstore <- return $ Data.Map.insert loc (vtype, varReg) (varsStore state)
    put $ StateLLVM (nextRegister state) (nextLabel state) (nextBlock state) (fnName state) (globalVarsDefs state)
                    newstore (currentLabel state)
    return (env', varReg)

getVar :: Ident -> Result (TType, String)
getVar vident = do
  env <- ask
  state <- get
  maybeLoc <- return $ Data.Map.lookup vident env
  case maybeLoc of
    (Just loc) -> do
      maybeVTypeReg <- return $ Data.Map.lookup loc (varsStore state)
      case maybeVTypeReg of
        (Just (vtype, vreg)) -> return (vtype, vreg)
        Nothing -> throwError "Loc not found"
    Nothing -> throwError $ "Undeclared variable in env " ++ (show vident)

updateCurrentLabel :: String -> Result String
updateCurrentLabel labelName = do
  state <- get
  nextLabelNum <- return $ nextLabel state
  label <- return $ labelName ++ "."++ (show nextLabelNum)
  put $ StateLLVM (nextRegister state) (nextLabelNum + 1) (nextBlock state) (fnName state) (globalVarsDefs state)
                  (varsStore state) label
  return label

appendJmpIfNotTerminated :: TStatus -> Instructions -> Instructions -> Result Instructions
appendJmpIfNotTerminated (Terminated) instrs jmpInstr = return $ instrs
appendJmpIfNotTerminated _ instrs jmpInstr = return $ instrs ++ jmpInstr

getCurrentLabel :: Result String
getCurrentLabel = do
  state <- get
  return $ currentLabel state

getDefaultValExpr :: TType -> TExpr
getDefaultValExpr (Int _) = ELitInt (Just (0, 0)) 0
getDefaultValExpr (Str _) = EString (Just (0, 0)) "\"\""
getDefaultValExpr (Bool _) = ELitFalse (Just (0, 0))

simplifyConstExpr :: TExpr -> Maybe Bool
simplifyConstExpr (ELitTrue _) = Just True
simplifyConstExpr (ELitFalse _) = Just False
simplifyConstExpr _ = Nothing