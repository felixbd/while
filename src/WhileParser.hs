{-# LANGUAGE LambdaCase #-}

-- | Parser for the while programming language in haskell - (c) 2023 Felix Drees

module WhileParser
  (Token(..)
  ,WhileAST(..)
  ,Expression(..)
  ,tokenize
  ,readFileContent
  ,mainParser
  ,runParser
  ,many
  ) where

import Control.Applicative
import Data.Char (isDigit, isAlpha)
import Data.List (stripPrefix)

{-|
Tokens for representing the while-language as haskell data types
relevant for the parsers tokenizer to generate the ast (Abstract Syntacs Tree) Tokens
-}
data Token = VarToken { getVarName :: String }  -- e.g. x_1 (has to start with letter - can contain digit and '_')
           | ConstToken { getConstInt :: Int }   -- e.g. 5 (positive int with 0 only)
           | AssignToken      -- :=
           | SemicolonToken   -- ; (for sequential commands)
           | NotEqualToken    -- !=
           | PlusToken        -- +
           | MinusToken       -- -
           | LoopToken        -- loop
           | WhileToken       -- while
           | DoToken          -- do
           | EndToken         -- end
           deriving (Show, Eq)

type LineNum = Int
data MetaToken = MetaToken { getLineNumber :: LineNum,
                             getToken :: Token
                           } deriving (Show, Eq)

-- | Data type to represent the WhileAST Abstract Syntax Tree (AST).
data WhileAST = Assignment String Expression    --  Assignment of a variable with an expression
              | Sequential WhileAST WhileAST    --  Sequential composition of two statements
              | While Expression WhileAST       --  While loop with a condition and a body
              | Loop Expression WhileAST        --  Normal loop with a fix iteration number and a body
              deriving (Show, Eq)

-- | Data type to represent expressions within WhileAST.
data Expression = Constant { getConst :: Int }                    --  Integer constant
                | Variable { getVar :: String }                 --  Variable reference
                | Add Expression Expression       --  Addition operation
                | Subtract Expression Expression  --  Subtraction operation
                | Neq Expression Expression       --  != operation
                deriving (Show, Eq)

newtype Parser a = Parser { runParser :: [MetaToken] -> Maybe ([MetaToken], a) }

instance Functor Parser where
  -- fmap for (<$>)
  fmap f (Parser p) = Parser helper
    where
      -- helper :: [Token] -> Maybe ([Token], Token)
      helper tx = p tx >>= \(ty, x) -> Just (ty, f x)


-- requires instance functor
instance Applicative Parser where
  pure x = Parser $ \xs -> Just (xs, x)

  (Parser pl) <*> (Parser pr) = Parser combinedParser
  -- <* :: f a -> f b -> f a
  -- *> :: f a -> f b -> f b
    where
      -- combinedParser :: [Token] -> Maybe ([Token], Token)
      combinedParser tx = pl tx >>= \(ty, f) -> pr ty >>= \(tz, t) -> Just (tz, f t)


instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser pl) <|> (Parser pr) = Parser $ \x -> pl x <|> pr x  -- since Maybe implements Alternative ...


tokenize :: [(LineNum, String)] -> [MetaToken]
tokenize = concatMap $ uncurry (flip tokenizeHelper)

tokenizeHelper :: String -> LineNum -> [MetaToken]
tokenizeHelper [] _ = []
tokenizeHelper cx@(c:cs) ln
  | c `elem` " \n\t\r" = tokenizeHelper cs ln -- Ignore whitespace and newlines
  | isAlpha c = uncurry keywordToken $ span (\x -> isAlpha x || isDigit x || x == '_') cx
  | isDigit c = (\(x, y) -> MetaToken ln (ConstToken (read x)) : tokenizeHelper y ln) $ span isDigit cx
  | c == ':', Just rest <- stripPrefix "=" cs = MetaToken ln AssignToken : tokenizeHelper rest ln
  | c == ';' = MetaToken ln SemicolonToken : tokenizeHelper cs ln
  | c == '!', Just rest <- stripPrefix "=" cs = MetaToken ln NotEqualToken : tokenizeHelper rest ln
  | c == '+' = MetaToken ln PlusToken : tokenizeHelper cs ln
  | c == '-' = MetaToken ln MinusToken : tokenizeHelper cs ln
  | otherwise = error $ "Invalid char: " ++ [c] ++ " in line: " ++ show ln
  where
    keywordToken :: String -> String -> [MetaToken]
    keywordToken token rest = case token of
      "loop"  -> MetaToken ln LoopToken : tokenizeHelper rest ln
      "while" -> MetaToken ln WhileToken : tokenizeHelper rest ln
      "do"    -> MetaToken ln DoToken : tokenizeHelper rest ln
      "end"   -> MetaToken ln EndToken : tokenizeHelper rest ln
      _       -> MetaToken ln (VarToken token) : tokenizeHelper rest ln


tokenParser :: MetaToken -> Parser MetaToken
tokenParser t = Parser $ \case
  (x:xs) | case (getToken x, getToken t) of
             (VarToken _, VarToken _) -> True
             (ConstToken _, ConstToken _) -> True
             _ -> getToken x == getToken t
         -> Just (xs, x)  -- NOTE: x is the token we are looking for (t is just of the same type)
  _ -> Nothing


-- tokenListParser :: [MetaToken] -> Parser [MetaToken]
-- tokenListParser = sequenceA . map tokenParser
-- tokenListParser = traverse tokenParser


-- Read the content of a file and return it as a list of its lines with numbering
readFileContent :: FilePath -> IO [(LineNum, String)]
readFileContent filePath = readFile filePath >>= \c -> return $ zip [1..] (lines c)


-- begin parser expression --------------------------------------------------

constIntParser :: Parser Expression
constIntParser = f <$> tokenParser (MetaToken 0 (ConstToken 0))
  where
    f :: MetaToken -> Expression
    f x = Constant $ (getConstInt . getToken) x

varNameParser :: Parser Expression
varNameParser = f <$> tokenParser (MetaToken 0 (VarToken ""))
  where
    -- f :: MetaToken -> Expression
    f x = Variable $ (getVarName . getToken) x


varConstParser :: MetaToken -> Parser Expression
varConstParser mt = case getToken mt of
    PlusToken -> Add <$> varNameParser <*> (tokenParser mt *> constIntParser)
    MinusToken -> Subtract <$> varNameParser <*> (tokenParser mt *> constIntParser)
    NotEqualToken -> Neq <$> varNameParser <*> (tokenParser mt *> constIntParser)

constVarParser :: MetaToken -> Parser Expression
constVarParser mt = case getToken mt of
    PlusToken -> Add <$> constIntParser <*> (tokenParser mt *> varNameParser)
    MinusToken -> Subtract <$> constIntParser <*> (tokenParser mt *> varNameParser)
    NotEqualToken -> Neq <$> constIntParser <*> (tokenParser mt *> varNameParser)

opExpressionParser :: Token -> Parser Expression
opExpressionParser op = constVarParser (MetaToken 0 op) <|> varConstParser (MetaToken 0 op)


addExpressionParser :: Parser Expression
addExpressionParser = opExpressionParser PlusToken

subtractExpressionParser :: Parser Expression
subtractExpressionParser = opExpressionParser MinusToken

neqExpressionParser :: Parser Expression
neqExpressionParser = opExpressionParser NotEqualToken

expressionParser :: Parser Expression
expressionParser = addExpressionParser <|> subtractExpressionParser <|> neqExpressionParser <|> constIntParser <|> varNameParser

-- end parser expression ----------------------------------------------------


-- begin parser WhileAST ----------------------------------------------------

assignmentParser :: Parser WhileAST
assignmentParser = Assignment . getVar <$> (varNameParser <* tokenParser (MetaToken 0 AssignToken)) <*> expressionParser

whileParser :: Parser WhileAST
whileParser = While <$> (tokenParser (MetaToken 0 WhileToken) *> expressionParser <* tokenParser (MetaToken 0 DoToken)) <*> mainParser <* tokenParser (MetaToken 0 EndToken)

loopParser :: Parser WhileAST
loopParser = Loop <$> (tokenParser (MetaToken 0 LoopToken) *> expressionParser <* tokenParser (MetaToken 0 DoToken)) <*> mainParser <* tokenParser (MetaToken 0 EndToken)

sequenceParser :: Parser WhileAST
sequenceParser = Sequential <$> (assignmentParser <|> whileParser <|> loopParser) <*> (tokenParser (MetaToken 0 SemicolonToken) *> mainParser)

mainParser :: Parser WhileAST
mainParser = sequenceParser <|> assignmentParser <|> whileParser <|> loopParser

-- end parser WhileAST ------------------------------------------------------



















