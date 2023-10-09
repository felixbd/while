-- | Parser for the while programming language in haskell - (c) 2023 Felix Drees

module WhileParser
  (Token(..)
  ,WhileAST(..)
  ,Expression(..)
  ,tokenize
  ,readFileContent
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
type ColumnNum = Int
data MetaToken = MetaToken { getLineNumber :: LineNum,
                             getColumnNumber :: ColumnNum,
                             getToken :: Token
                           } deriving (Show, Eq)

-- | Data type to represent the WhileAST Abstract Syntax Tree (AST).
data WhileAST = Assignment String Expression    --  Assignment of a variable with an expression
              | Sequential WhileAST WhileAST    --  Sequential composition of two statements
              | While Expression WhileAST       --  While loop with a condition and a body
              | Loop Expression WhileAST        --  Normal loop with a fix iteration number and a body
              | Skip                            --  A skip statement, representing no operation
              deriving (Show, Eq)

-- | Data type to represent expressions within WhileAST.
data Expression = Constant { getConst :: Int }                    --  Integer constant
                | Variable { getVar :: String }                 --  Variable reference
                | Add Expression Expression       --  Addition operation
                | Subtract Expression Expression  --  Subtraction operation
                | Neq Expression Expression       --  != operation
                deriving (Show, Eq)


-- TODO(felix): upgrade current verwion to handel errors better
--  newtype TokenParser a = TokenParser {
--          runTokenParser :: [MetaToken] -> Either (LineNum, ColNum, String) ([MetaToken], a)
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
    where
      -- combinedParser :: [Token] -> Maybe ([Token], Token)
      combinedParser tx = pl tx >>= \(ty, f) -> pr ty >>= \(tz, t) -> Just (tz, f t)


instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser pl) <|> (Parser pr) = Parser $ \x -> pl x <|> pr x  -- since Maybe implements Alternative ...


{-|
        Tokenizer-Funktion
Function for converting a given String (while programm sorce code)
to a list of while programm tokens.
-}
tokenize :: [(LineNum, String)] -> [MetaToken]
tokenize = concat . map (uncurry (flip tokenizeHelper))

-- TODO(felix): add column number
tokenizeHelper :: String -> LineNum -> [MetaToken]
tokenizeHelper [] _ = []
tokenizeHelper cx@(c:cs) ln
  | c `elem` " \n\t\r" = tokenizeHelper cs ln -- Ignoriere Leerzeichen und Zeilenumbrüche
  | isAlpha c = uncurry keywordToken $ span (\x -> isAlpha x || isDigit x || x == '_') cx
  | isDigit c = uncurry (\x y -> (MetaToken ln 0 (ConstToken (read x))) : tokenizeHelper y ln) $ span isDigit cx
  | c == ':', Just rest <- stripPrefix "=" cs = MetaToken ln 0 AssignToken : tokenizeHelper rest ln
  | c == ';' = MetaToken ln 0 SemicolonToken : tokenizeHelper cs ln
  | c == '!', Just rest <- stripPrefix "=" cs = MetaToken ln 0 NotEqualToken : tokenizeHelper rest ln
  | c == '+' = MetaToken ln 0 PlusToken : tokenizeHelper cs ln
  | c == '-' = MetaToken ln 0 MinusToken : tokenizeHelper cs ln
  | otherwise = error $ "Invalid char: " ++ [c] ++ " in line: " ++ show ln
  where
    keywordToken :: String -> String -> [MetaToken]
    keywordToken token rest = case token of
      "loop"  -> MetaToken ln 0 LoopToken : tokenizeHelper rest ln
      "while" -> MetaToken ln 0 WhileToken : tokenizeHelper rest ln
      "do"    -> MetaToken ln 0 DoToken : tokenizeHelper rest ln
      "end"   -> MetaToken ln 0 EndToken : tokenizeHelper rest ln
      _       -> MetaToken ln 0 (VarToken token) : tokenizeHelper rest ln


tokenParser :: MetaToken -> Parser MetaToken
tokenParser t = Parser $ \input -> case input of
  (x:xs) | case (getToken x, getToken t) of
             (VarToken _, VarToken _) -> True
             (ConstToken _, ConstToken _) -> True
             _ -> getToken x == getToken t
         -> Just (xs, t)
  _ -> Nothing


tokenListParser :: [MetaToken] -> Parser [MetaToken]
tokenListParser = sequenceA . map tokenParser

-- Read the content of a file and return it as a list of its lines with numbering
readFileContent :: FilePath -> IO [(LineNum, String)]
readFileContent filePath = readFile filePath >>= \c -> return $ zip [1..] (lines c)


constIntParser :: Parser Expression
constIntParser = f <$> tokenParser (MetaToken 0 0 (ConstToken 0))
  where
    f x = Constant $ (getConstInt . getToken) x

varNameParser :: Parser Expression
varNameParser = f <$> tokenParser (MetaToken 0 0 (VarToken ""))
  where
    f x = Variable $ (getVarName . getToken) x

addExpressionParser :: Parser Expression
addExpressionParser = f <$> (varConst <|> constVar <|> constConst <|> varVar)
  where
    f (_:a:b:xs) = case (getToken a, getToken b) of
                     (ConstToken x, ConstToken y) -> Add (Constant x) (Constant y)
                     _ -> undefined

    var   = MetaToken 0 0 (VarToken "")
    const = MetaToken 0 0 (ConstToken 0)
    add   = MetaToken 0 0 (PlusToken)

    varConst = tokenListParser [var, add, const]
    constVar = tokenListParser [const, add, var]
    constConst = tokenListParser [const, add, const]
    varVar = tokenListParser [var, add, var]

subtractExpressionParser :: Parser Expression
subtractExpressionParser = undefined

neqExpressionParser :: Parser Expression
neqExpressionParser = undefined

expressionParser :: Parser Expression
expressionParser = constIntParser
  <|> varNameParser
  <|> addExpressionParser
  <|>subtractExpressionParser
  <|> neqExpressionParser



--
