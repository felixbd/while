-- module Main where

import Test.Tasty
import Test.Tasty.HUnit


summe :: Integer -> Integer
summe n = if n <= 0 then 0 else div (n * (n + 1)) 2


-- main :: IO ()
main = defaultMain $ testGroup "0. ./src/WhileParser.hs" [tokenizerTest0, parserTest0]

tokenizerTest0 = testGroup "tokenizer" [
    testCase "summe 0" $ summe 0 @?= 0
  , testCase "summe 5" $ summe 5 @?= 15
  ]

parserTest0 = testGroup "parser" [
    testCase "parse asignment" $ summe 0 @?= 0
  ]
