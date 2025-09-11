-- | Parser and interpreter for the While programming language
-- | (c) 2023 Felix Drees - BSD3 License

module Main (main) where

import System.Environment (getArgs)

import WhileParser (tokenize, readFileContent, runASTParser)  -- WhileAST(..),
import WhileEvaluation (eval, evalT, evalM, getVarState, runEvalM)
  -- VarName, VarVal, VarState, VarStateWorld)


-- | Define a data type to represent the command-line options
newtype Options = Options { getFileName :: FilePath } deriving Show


-- | Parse command-line arguments manually
parseArgs :: [String] -> Either String Options
parseArgs [file] = Right $ Options file
parseArgs _      = Left "Usage: stack run <filename>"

-- | Run interpreter logic with the parsed options
runInterpreter :: Options -> IO ()
runInterpreter options = do
  putStrLn $ "\n" ++ concat (replicate 40 "= ") ++ "=\n"

  putStrLn $ "\ESC[92m[Interpreting file]\ESC[0m    " ++ getFileName options

  --      ast              tokens       lines of code     file name
  ast <- runASTParser . tokenize <$> (readFileContent . getFileName) options

  -- print $ eval ast []  -- eval v1
  -- print $ evalT ast []  -- eval v2

  let program = evalM ast  -- Monad eval
  print $ getVarState program []

  -- eg.
  --  let program2 = evalM ast_1 >> evalM ast_2 >> ...
  --  print $ getVarState program2 []

  -- or use:
  --  print $ runEvalM ast []

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
