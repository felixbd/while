-- | Parser and interpreter for the While programming language
-- | (c) 2023 Felix Drees - BSD3 License

module Main (main) where

import System.Environment (getArgs)

import WhileParser (tokenize, readFileContent, runASTParser)  -- WhileAST(..),
import WhileEvaluation (eval) --, evalM, VarName, VarVal, VarState, VarStateWorld)


-- | Define a data type to represent the command-line options
newtype Options = Options { getFileName :: FilePath }


-- | Parse command-line arguments manually
parseArgs :: [String] -> Either String Options
parseArgs [file] = Right $ Options file
parseArgs _      = Left "Usage: stack run <filename>"

-- | Run interpreter logic with the parsed options
runInterpreter :: Options -> IO ()
runInterpreter options = do
  putStrLn $ "\n" ++ concat (replicate 40 "= ") ++ "=\n"

  putStrLn $ "\ESC[92m[Interpreting file]\ESC[0m    " ++ getFileName options

  -- Read the content of the specified file
  whileLines <- readFileContent $ getFileName options
  let tokens = tokenize whileLines

  putStrLn "\ESC[92m[DONE TOKENIZE]\ESC[0m"
  -- print tokens

  let ast = runASTParser tokens
  putStrLn "\ESC[92m[DONE PARSING AST]\ESC[0m"
  -- print ast

  putStrLn "\ESC[92m[RESULT OF EVALUATION]\ESC[0m"
  let a = eval ast []
  print a

  putStrLn $ "\n" ++ concat (replicate 40 "= ") ++ "=\n"


main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> putStrLn $ vbar ++ "\ESC[91m[Error]\ESC[0m: " ++ err ++
      "\nUse 'stack run <filename>' to interpret a while source file." ++ vbar
    Right options -> runInterpreter options
  where
    vbar = "\n" ++ concat (replicate 40 "= ") ++ "=\n"
