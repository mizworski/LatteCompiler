module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.Environment
import System.Directory
import Data.List

import Frontend.ErrM
import Frontend.ParLatte


main :: IO ()
main = do
  putStrLn ""
  putStrLn "------------------"

  putStrLn "Testing programs with erros."
  badDir <- return "test/bad"
  badFilenames <- listDirectory badDir
  compileFiles $ [badDir ++ "/" ++ fn | fn <- badFilenames]

  putStrLn "------------------"

--   putStrLn "Testing correct programs"
--   goodDir <- return "test/good"
--   goodFilenames <- listDirectory goodDir
--   compileFiles $ [goodDir ++ "/" ++ fn | fn <- goodFilenames, ".lat" `isSuffixOf` fn]

  putStrLn "------------------"


compileFile :: String -> IO()
compileFile filename = do
  program <- readFile filename
  tokenized <- return $ myLexer program
  case (pProgram tokenized) of
    (Ok p) -> do
      putStr $ unlines $ lines program
      putStrLn $ show p
    (Bad p) -> do
      putStrLn $ filename ++ p

compileFiles :: [FilePath] -> IO()
compileFiles ([]) = return ()
compileFiles (fp:fps) = do
  putStrLn "------------------"
  putStrLn fp
  compileFile fp
  compileFiles fps