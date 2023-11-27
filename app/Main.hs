-- | Parser and interpreter for the While programming language
-- | (c) 2023 Felix Drees - BSD3 License

module Main (main) where

import WhileParser (tokenize, readFileContent, runASTParser)  -- WhileAST(..),
import WhileEvaluation (eval) --, evalM, VarName, VarVal, VarState, VarStateWorld)


main :: IO ()
main = do
  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"

  -- Prompt the user for the file name
  putStrLn "Enter the file name:"
  fileName <- getLine

  -- Read the content of the specified file
  whileLines <- readFileContent fileName  -- "./examples/simple.while"
  let tokens = tokenize whileLines

  putStrLn "\n\ESC[92m[DONE TOKENIZE]\ESC[0m:"
  print tokens

  let astOfFoo = runASTParser tokens
  putStrLn "\n\ESC[92m[DONE PARSING AST]\ESC[0m:"
  print astOfFoo

  putStrLn "\n\ESC[92m[OUTPUT OF EVALUATION]\ESC[0m:"
  let a = eval astOfFoo []
  print a

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"
