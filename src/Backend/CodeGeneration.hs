module Backend.CodeGeneration where

import Control.Monad.State

import Backend.Datatypes
import Frontend.AbsLatte


irToCode :: TProgram -> Result StateLLVM String
irToCode (Program _ stmts) = do
  program <- return $ unlines ["define i32 @main() {", "ret i32 0", "}", ""]
  functionDefs <- return [program]
  return $ unlines functionDefs