module Main where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map
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
  putStrLn program
  case (pProgram $ myLexer program) of
    (Ok ir) -> do
      typeCheckRes <- runExceptT $ semanticAnalysis ir
      case typeCheckRes of
        (Left errMsg) -> do
          -- syntax error
          hPutStrLn stderr "ERROR"
          hPutStrLn stderr $ inputFilename ++ errMsg
          exitWith (ExitFailure 42)
        (Right optIR) -> do
          hPutStrLn stderr "OK"
          maybeRes <- runExceptT $ runReaderT (runStateT (emitProgram optIR) initialState) Data.Map.empty
          case maybeRes of
            (Left errMsg) -> do
              hPutStrLn stderr "COMPILER INTERNAL ERROR"
              hPutStrLn stderr $ errMsg
              exitWith (ExitFailure 42)
            (Right (llvmCode, _)) -> do
              putStrLn llvmCode
              writeFile outputFilename llvmCode
              return ()
    (Bad errMsg) -> do
      -- parser error
      hPutStrLn stderr "ERROR"
      hPutStrLn stderr $ inputFilename ++ errMsg
      exitWith (ExitFailure 42)


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