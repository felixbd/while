module Main (main) where

import Lib
import WhileParser (tokenize)



main :: IO ()
main = do
  someFunc
  print $ tokenize [(1, "x_1 := 10;\n"),
                    (2, "while x != 0 do\n"),
                    (3, "    x := x - 1\n"),
                    (4, "end")]
