module Main (main) where

import WhileParser (tokenize, readFileContent, mainParser, runParser, many)


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

  print $ show (runParser (many mainParser) tokens1)

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"

  print tokens2

  putStrLn $ "\n" ++ concat (replicate 40 " =") ++ "\n"

  print $ show (runParser (many mainParser) tokens2)
