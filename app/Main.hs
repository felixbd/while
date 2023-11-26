-- | Parser and interpreter for the While programming language
-- | (c) 2023 Felix Drees - BSD3 License

module Main (main) where

import WhileParser (WhileAST(..), tokenize, readFileContent, runASTParser)

import WhileEvaluation ()  -- eval)  -- , calcInitialVarStates, isSimpleArithmeticExpression)


main :: IO ()
main = do

  whileLines <- readFileContent "./examples/foo.while"
  let tokens1 = tokenize whileLines

  print tokens1

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"

  print $ runASTParser tokens1

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"

  -- let x = runASTParser tokens1

  -- print $ calcInitialVarStates x

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"
