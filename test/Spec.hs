-- module Main where

import Test.Tasty
import Test.Tasty.HUnit

-- import Sum

summe :: Integer -> Integer
summe n = if n <= 0 then 0 else div (n * (n + 1)) 2


main = defaultMain $ testGroup "0. Übungsblatt" [exercise0]

exercise0 = testGroup "Summe" [
    testCase "summe 0" $ summe 0 @?= 0
  , testCase "summe 5" $ summe 5 @?= 15
  ]


-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
