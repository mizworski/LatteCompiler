module Main where

import Control.Monad.Except
import Data.List
import System.Environment
import System.Directory
import Test.Tasty
import Test.Tasty.HUnit

import Frontend.ErrM
import Frontend.ParLatte
import Frontend.SemanticAnalysis


main :: IO ()
main = do
  putStrLn ""
  putStrLn "------------------"

  putStrLn "Testing programs with erros."
  badDir <- return "test/bad"
  badFilenames <- listDirectory badDir
  compileFiles $ sort $ [badDir ++ "/" ++ fn | fn <- badFilenames]

  putStrLn "------------------"

  putStrLn "Testing correct programs"
  goodDir <- return "test/good"
  goodFilenames <- listDirectory goodDir
  compileFiles $ sort $  [goodDir ++ "/" ++ fn | fn <- goodFilenames, ".lat" `isSuffixOf` fn]

  putStrLn "------------------"

compileFile :: String -> IO()
compileFile filename = do
  program <- readFile filename
  tokenized <- return $ myLexer program
  case (pProgram tokenized) of
    (Ok p) -> do
      typeCheckRes <- runExceptT $ semanticAnalysis p
      case typeCheckRes of
        (Left errMsg) -> do
          putStrLn $ filename ++ errMsg
        otherwise -> do
          putStrLn "Semantic analysis passed."
          return()

    (Bad p) -> do
      putStrLn $ filename ++ p

compileFiles :: [FilePath] -> IO()
compileFiles ([]) = return ()
compileFiles (fp:fps) = do
  putStrLn "------------------"
  putStrLn fp
  compileFile fp
  compileFiles fps