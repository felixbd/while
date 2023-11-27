-- | Parser and interpreter for the While programming language
-- | (c) 2023 Felix Drees - BSD3 License

module Main (main) where

import WhileParser (WhileAST(..), tokenize, readFileContent, runASTParser)
import WhileEvaluation (eval, evalM, VarName, VarVal, VarState, VarStateWorld)


main :: IO ()
main = do

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"

  whileLines <- readFileContent "./examples/simple.while"
  let tokens = tokenize whileLines

  print tokens

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"

  let astOfFoo = runASTParser tokens
  print astOfFoo

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"

  putStrLn "\ESC[92m[OUTPUT]\ESC[0m:"

  let a = eval astOfFoo []
  print a

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"
