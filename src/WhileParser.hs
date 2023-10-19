-- | Parser for the while programming language in haskell - (c) 2023 Felix Drees

module WhileParser
  (Token(..)
  ,WhileAST(..)
  ,Expression(..)
  ,tokenize
  ,readFileContent
  ,whileASTParser
  ,runParser
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


-- TODO(felix): upgrade current version to handel errors better
--  newtype TokenParser a = TokenParser {
--          runTokenParser :: [MetaToken] -> Either (LineNum, String) ([MetaToken], a)
--    }
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
  empty = Parser $ \_ -> Nothing
  (Parser pl) <|> (Parser pr) = Parser $ \x -> pl x <|> pr x  -- since Maybe implements Alternative ...


{-|
        Tokenizer-Funktion
Function for converting a given String (while program source code)
to a list of while program tokens.
-}
tokenize :: [(LineNum, String)] -> [MetaToken]
tokenize = concat . map (uncurry (flip tokenizeHelper))

tokenizeHelper :: String -> LineNum -> [MetaToken]
tokenizeHelper [] _ = []
tokenizeHelper cx@(c:cs) ln
  | c `elem` " \n\t\r" = tokenizeHelper cs ln -- Ignore whitespace and newlines
  | isAlpha c = uncurry keywordToken $ span (\x -> isAlpha x || isDigit x || x == '_') cx
  | isDigit c = uncurry (\x y -> (MetaToken ln (ConstToken (read x))) : tokenizeHelper y ln) $ span isDigit cx
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
tokenParser t = Parser $ \input -> case input of
  (x:xs) | case (getToken x, getToken t) of
             (VarToken _, VarToken _) -> True
             (ConstToken _, ConstToken _) -> True
             _ -> getToken x == getToken t
         -> Just (xs, x)  -- NOTE: x is the token we are looking for (t is just of the same type)
  _ -> Nothing

tokenListParser :: [MetaToken] -> Parser [MetaToken]
tokenListParser = sequenceA . map tokenParser

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


operatorExpressionParser :: Token -> Parser Expression
operatorExpressionParser t = f <$> (varConst <|> constVar <|> constConst <|> varVar)
  where
    -- f :: MetaToken -> Expression
    f (_:a:b:xs) = case (getToken a, getToken b) of
                     (ConstToken x, ConstToken y) -> g (Constant x) (Constant y)
                     (ConstToken x, VarToken y)   -> g (Constant x) (Variable y)
                     (VarToken x, ConstToken y)   -> g (Variable x) (Constant y)
                     (VarToken x, VarToken y)     -> g (Variable x) (Variable y)
                     _ -> undefined

    g = case t of
      PlusToken     -> Add
      MinusToken    -> Subtract
      NotEqualToken -> Neq
      _             -> error "[INVALID TOKEN] operatorExpressionParser"

    var   = MetaToken 0 (VarToken "")
    myConst = MetaToken 0 t
    add   = MetaToken 0 (PlusToken)

    varConst = tokenListParser [var, add, myConst]
    constVar = tokenListParser [myConst, add, var]
    constConst = tokenListParser [myConst, add, myConst]
    varVar = tokenListParser [var, add, var]


addExpressionParser :: Parser Expression
addExpressionParser = operatorExpressionParser PlusToken

subtractExpressionParser :: Parser Expression
subtractExpressionParser = operatorExpressionParser MinusToken

neqExpressionParser :: Parser Expression
neqExpressionParser = operatorExpressionParser NotEqualToken

expressionParser :: Parser Expression
expressionParser = constIntParser
  <|> varNameParser
  <|> addExpressionParser
  <|> subtractExpressionParser
  <|> neqExpressionParser

-- end parser expression ----------------------------------------------------


-- begin parser WhileAST ----------------------------------------------------

assignmentParser :: Parser WhileAST
assignmentParser = f <$> (varNameParser <* tokenParser (MetaToken 0 AssignToken)) <*> expressionParser
  where
    f :: Expression -> Expression -> WhileAST
    f x y = Assignment (getVar x) y

sequenceParser :: Parser WhileAST
sequenceParser = f <$> (tokenParser (MetaToken 0 SemicolonToken) *> whileASTParser) <*> whileASTParser
  where
    f :: WhileAST -> WhileAST -> WhileAST
    f x y = Sequential x y

loopParser :: Parser WhileAST
loopParser = f <$> (tokenParser (MetaToken 0 LoopToken) *> expressionParser) <*> (tokenParser (MetaToken 0 DoToken) *> whileASTParser)
  where
    f :: Expression -> WhileAST -> WhileAST
    f x y = Loop x y

whileParser :: Parser WhileAST
whileParser = f <$> (tokenParser (MetaToken 0 WhileToken) *> expressionParser) <*> (tokenParser (MetaToken 0 DoToken) *> whileASTParser)
  where
    f :: Expression -> WhileAST -> WhileAST
    f x y = While x y


whileASTParser :: Parser WhileAST
whileASTParser = assignmentParser <|> sequenceParser <|> loopParser <|> whileParser


-- end parser WhileAST ------------------------------------------------------
