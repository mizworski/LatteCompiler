module Backend.CodeGeneration where

import Control.Monad.Except
import Control.Monad.State
import Data.List

import Backend.Datatypes
import Frontend.AbsLatte


emitProgram :: TProgram -> Result StateLLVM String
emitProgram (Program _ stmts) = do
  predefinedFns <- emitPredefinedFnDecls
  functionDefs <- sequence $ map emitFunction stmts
  return $ unlines $ predefinedFns ++ [""] ++ functionDefs

emitPredefinedFnDecls :: Result StateLLVM [String]
emitPredefinedFnDecls =
  return $ ["declare void @printInt(i32)",
            "declare void @printString(i8*)",
            "declare i32 @readInt()",
            "declare i8* @readString()",
            "declare void @error()"]

emitFunction :: TTopDef -> Result StateLLVM String
emitFunction (FnDef _ rtype (Ident fname) args body) = do
  header <- emitHeader rtype fname args
  entry <- emitEntry args
  body <- emitBlock body
  footer <- return ["}"]
  return $ unlines $ header ++ entry ++ body ++ footer

emitHeader :: TType -> String -> [TArg] -> Result StateLLVM [String]
emitHeader rtype fname args =
  return ["define " ++ (llvmType rtype) ++ " @" ++ fname ++ "(" ++ (emitArguments args) ++ ") {"]

emitEntry :: [TArg] -> Result StateLLVM [String]
emitEntry args = return ["entry:"]

emitBlock :: TBlock -> Result StateLLVM [String]
emitBlock (Block _ []) = return ["ret i32 0"]
emitBlock (Block pos (stmt:stmts)) = do
  stmtCode <- emitStmt stmt
  stmtsCode <- emitBlock (Block pos stmts)
  return $ stmtCode ++ stmtsCode

emitStmt :: TStmt -> Result StateLLVM [String]
emitStmt _ = return []

emitArguments :: [TArg] -> String
emitArguments args = intercalate ", " (emitArguments' args) where
  emitArguments' [] = []
  emitArguments' ((Arg _ atype (Ident aname)):args) = ((llvmType atype) ++ " %" ++ aname) : (emitArguments' args)

llvmType :: TType -> String
llvmType (Int _) = "i32"
llvmType (Str _) = "i8*"
llvmType (Bool _) = "i1"
llvmType (Void _) = "void"
llvmType _ = "**err**"