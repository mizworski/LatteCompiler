module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.Environment

import Frontend.ErrM
import Frontend.ParLatte


main :: IO ()
main = do
  putStrLn ""
  putStrLn "------------------"
  putStrLn "Testing programs with erros."
  compileFile "test/bad/bad001.lat"

  putStrLn "------------------"
  putStrLn "Testing correct programs."
  compileFile "test/good/core002.lat"

  putStrLn "------------------"


compileFile :: String -> IO()
compileFile filename = do
  program <- readFile filename
  tokenized <- return $ myLexer program
  case (pProgram tokenized) of
    (Ok p) -> do
      putStrLn $ show p
    (Bad p) -> do
      putStrLn $ show tokenized
      putStrLn p