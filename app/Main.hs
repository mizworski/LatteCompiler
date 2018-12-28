module Main where

import Control.Monad.Except
import Control.Monad.State
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Process

import Backend.CodeGeneration
import Backend.Datatypes
import Frontend.ErrM
import Frontend.ParLatte
import Frontend.SemanticAnalysis


compileToLLVM :: String -> String -> IO ()
compileToLLVM inputFilename outputFilename = do
  program <- readFile inputFilename
  case (pProgram $ myLexer program) of
    (Ok ir) -> do
      typeCheckRes <- runExceptT $ semanticAnalysis ir
      case typeCheckRes of
        (Left errMsg) -> do
          -- syntax error
          hPutStrLn stderr "ERROR"
          hPutStrLn stderr $ inputFilename ++ errMsg
          exitWith (ExitFailure 42)
        otherwise -> do
          -- semantic analysis passed
          hPutStrLn stderr "OK"
--           ir <- middleEnd ir
          (llvmCode, _) <- runStateT (irToCode ir) initialState
          writeFile outputFilename llvmCode
          return ()
    (Bad errMsg) -> do
      -- parser error
      putStrLn $ inputFilename ++ errMsg


main :: IO()
main = do
  args <- getArgs
  case args of
    [inputFilename] -> do
      case (stripExtension ".lat" inputFilename) of
        (Just filenameWithoutExt) -> do
          llvmFilename <- return $ filenameWithoutExt ++ ".ll"
          bitcodeFilename <- return $ filenameWithoutExt ++ ".bc"
          compileToLLVM inputFilename llvmFilename
          callProcess "llvm-as" ["-o", ".tmp.bc", llvmFilename ]
          callProcess "llvm-as" ["-o", ".runtime.bc", "lib/runtime.ll"]
          callProcess "llvm-link" ["-o", bitcodeFilename, ".tmp.bc", ".runtime.bc"]
          callProcess "rm" [".tmp.bc", ".runtime.bc"]
          exitWith ExitSuccess
        Nothing -> do
          hPutStrLn stderr $ "Input filename has wrong extension."
          exitWith (ExitFailure 42)
    _ -> do
      hPutStrLn stderr $ "Wrong number of arguments passed to compiler."
      exitWith (ExitFailure 42)