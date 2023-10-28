-- | Parser and interpreter for the While programming language
-- | (c) 2023 Felix Drees - BSD3 License

module Main (main) where

import WhileParser (WhileAST(..), tokenize, readFileContent, runASTParser)

import WhileEvaluation (eval, calcInitialVarStates, isSimpleArithmeticExpression)


main :: IO ()
main = do

  whileLines <- readFileContent "./examples/foo.while"
  let tokens1 = tokenize whileLines

  let tokens2 = tokenize [(1, "x_1 := 10;\n"),
                          (2, "while x != 0 do\n"),
                          (3, "    x := x - 1\n"),
                          (4, "end;"),
                          (5, "loop 8 do i := i + 1 end")
                         ]

  print tokens1

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"

  print $ runASTParser tokens1

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"

  print tokens2

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"

  print $ show (runASTParser tokens2)

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"

  let x = runASTParser tokens1

  print $ calcInitialVarStates x

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"
