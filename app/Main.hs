module Main (main) where

import Lib
import WhileParser (tokenize)



main :: IO ()
main = do
  someFunc
  print $ tokenize "x_1 := 10; while x != 0 do x := x - 1 end"
