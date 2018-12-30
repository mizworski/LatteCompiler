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
  bodyInstrs <- emitBlock body
  body <- return $ ["    " ++ instr | instr <- bodyInstrs]
  footer <- return ["}"]
  return $ unlines $ header ++ entry ++ body ++ footer

emitHeader :: TType -> String -> [TArg] -> Result Instructions
emitHeader rtype fname args =
  return ["define " ++ (llvmType rtype) ++ " @" ++ fname ++ "(" ++ (emitArguments args) ++ ") {"]

emitEntry :: [TArg] -> Result Instructions
emitEntry args = return ["entry:"]

emitBlock :: TBlock -> Result Instructions
emitBlock (Block _ []) = return []
emitBlock (Block pos (stmt:stmts)) = do
  stmtCode <- emitStmt stmt
  stmtsCode <- emitBlock (Block pos stmts)
  return $ stmtCode ++ stmtsCode

emitStmt :: TStmt -> Result Instructions
emitStmt (Empty _) = return []
emitStmt (BStmt _ block) = emitBlock block
emitStmt (SExp _ expr) = do
  (instructions, _, _) <- emitExpr expr
  return instructions
emitStmt (Ret _ expr) = do
  (instructions, rtype, res) <- emitExpr expr
  return $ instructions ++ ["ret " ++ (llvmType rtype) ++ " " ++ res]
emitStmt (VRet _) = return ["ret void"]
emitStmt _ = return []

emitExpr :: TExpr -> Result (Instructions, TType, Register)
emitExpr (ELitInt _ num) = return ([], Int (Just (0,0)), show num)
emitExpr (EString _ str) = do
  reg <- getNextRegister
  state <- get
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

  
emitExpr _ = return ([], Int (Just (0, 0)), "%0")

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
  env <- ask
  env' <- local (const env) $ putFn (Ident "printInt") (Void (Just (0, 0)))
  env'' <- local (const env') $ putFn (Ident "printString") (Void (Just (0, 0)))
  env''' <- local (const env'') $ putFn (Ident "error") (Void (Just (0, 0)))
  env'''' <- local (const env''') $ putFn (Ident "readInt") (Int (Just (0, 0)))
  env''''' <- local (const env'''') $ putFn (Ident "readString") (Str (Just (0, 0)))
  return env'''''

putFnTypes ((FnDef _ rtype fname _ _):stmts) = do
  env <- ask
  env' <- local (const env) $ putFn fname rtype
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
llvmType _ = "**err**"