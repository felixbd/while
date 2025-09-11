-- | Evaluation of a while AST in haskell
-- | (c) 2023 Felix Drees - BSD3 License

-- module Main where

import Test.Tasty
import Test.Tasty.HUnit

import WhileParser
  (WhileAST(..)
  ,MetaToken(..)
  ,LineNum
  ,Token(..)
  ,Expression(..)
  ,tokenize
  ,readFileContent
  ,runASTParser
  )

import WhileEvaluation
  (evalM
  ,VarName
  ,VarVal
  ,VarState
  ,VarStateWorld
  )

main :: IO ()
main = defaultMain $ testGroup "0. ./src/WhileParser.hs" [tokenizerTest0, parserTest0]

tokenizerTest0 :: TestTree
tokenizerTest0 = testGroup "tokenizer" [
  testCase "tokenize list" $
    tokenize [(1, "x_1 := 10;\n"),
               (2, "while x != 0 do\n"),
               (3, "    x := x - 1\n"),
               (4, "end;"),
               (5, "loop 8 do i := i + 1 end")
               ]
    @?=
    [MetaToken 1 (VarToken "x_1"),
     MetaToken 1 AssignToken,
     MetaToken 1 (ConstToken 10),
     MetaToken 1 SemicolonToken,
     MetaToken 2 WhileToken,
     MetaToken 2 (VarToken "x"),
     MetaToken 2 NotEqualToken,
     MetaToken 2 (ConstToken 0),
     MetaToken 2 DoToken,
     MetaToken 3 (VarToken "x"),
     MetaToken 3 AssignToken,
     MetaToken 3 (VarToken "x"),
     MetaToken 3 MinusToken,
     MetaToken 3 (ConstToken 1),
     MetaToken 4 EndToken,
     MetaToken 4 SemicolonToken,
     MetaToken 5 LoopToken,
     MetaToken 5 (ConstToken 8),
     MetaToken 5 DoToken,
     MetaToken 5 (VarToken "i"),
     MetaToken 5 AssignToken,
     MetaToken 5 (VarToken "i"),
     MetaToken 5 PlusToken,
     MetaToken 5 (ConstToken 1),
     MetaToken 5 EndToken]

  , testCase "tokenize .while file" $ (show (10 + 10) :: String) @?= "20"
  -- whileLines <- readFileContent "./examples/foo.while"
  -- let tokens = tokenize whileLines
  ]

parserTest0 :: TestTree
parserTest0 = testGroup "parser" [
    testCase "AST parser" $
      runASTParser
        (tokenize
          [(1, "x_1 := 10;\n"),
           (2, "while x != 0 do\n"),
           (3, "    x := x - 1\n"),
           (4, "end;"),
           (5, "loop 8 do i := i + 1 end")])
      @?=
        Sequential
          (Assignment "x_1" (Constant 10))
          (Sequential
            (While
              (Neq (Variable "x") (Constant 0))
              (Assignment "x" (Subtract (Variable "x") (Constant 1))))
            (Loop
              (Constant 8) (Assignment "i" (Add (Variable "i") (Constant 1)))))
  ]
