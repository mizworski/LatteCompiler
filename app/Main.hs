module Main where

import Control.Monad.Except
import System.Environment
import System.Exit
import System.IO

import Frontend.ErrM
import Frontend.ParLatte
import Frontend.SemanticAnalysis


main :: IO()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      program <- readFile filename
      case (pProgram $ myLexer program) of
        (Ok p) -> do
          typeCheckRes <- runExceptT $ semanticAnalysis p
          case typeCheckRes of
            (Left errMsg) -> do
              hPutStrLn stderr "ERROR"
              hPutStrLn stderr $ filename ++ errMsg
              exitWith (ExitFailure 42)
            otherwise -> do
              hPutStrLn stderr "OK"
              putStrLn "Semantic analysis passed."
              exitWith ExitSuccess
        (Bad errMsg) -> do
          putStrLn $ filename ++ errMsg
    _ -> do
      hPutStrLn stderr $ "Wrong number of arguments passed to compiler."
      exitWith (ExitFailure 42)