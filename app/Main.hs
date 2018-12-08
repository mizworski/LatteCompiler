module Main where

import Control.Monad.Except
import System.Environment

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
          putStr $ unlines $ lines program
          typeCheckRes <- runExceptT $ semanticAnalysis p
          case typeCheckRes of
            (Left errMsg) -> do
              putStrLn $ filename ++ errMsg
            otherwise -> do
              putStrLn $ show p
              return()
        (Bad errMsg) -> do
          putStrLn $ filename ++ errMsg
    _ -> do
      putStrLn "Wrong number of arguments"