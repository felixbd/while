module Main (main) where

import Lib
import WhileParser (tokenize, readFileContent)


main :: IO ()
main = do
  someFunc

  lines <- readFileContent "./examples/foo.while"
  let tokens1 = tokenize lines

  let tokens2 = tokenize [(1, "x_1 := 10;\n"),
                          (2, "while x != 0 do\n"),
                          (3, "    x := x - 1\n"),
                          (4, "end")]

  print tokens1
  putStrLn $ "\n" ++ (concat (replicate 40 " =")) ++ "\n"
  print tokens2
