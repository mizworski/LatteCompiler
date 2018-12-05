module Main where

import Frontend.ErrM
import Frontend.ParLatte

import System.Environment

main :: IO()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      program <- readFile filename
      case (pProgram $ myLexer program) of
        (Ok p) -> do
          putStrLn $ show p
        (Bad errMsg) -> do
          putStrLn $ filename ++ errMsg
    _ -> do
      putStrLn "Wrong number of arguments"