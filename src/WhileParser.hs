-- | Parser for the while programming language in haskell - (c) 2023 Felix Drees

module WhileParser
  (Token(..)
  ,WhileAST(..)
  ,Expression(..)
  ,tokenize
  ) where

import System.IO
import Control.Monad
import Control.Applicative
import Data.Char (isDigit, isAlpha)
import Data.List (stripPrefix)


{-|
Tokens for representing the while-language as haskell data types
relevant for the parsers tokenizer to generate the ast (Abstract Syntacs Tree) Tokens
-}
data Token = VarToken String  -- e.g. x_1 (has to start with letter - can contain digit and '_')
           | ConstToken Int   -- e.g. 5 (positive int with 0 only)
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


-- TODO(felix): upgrade current verwion to handel errors better
--  newtype TokenParser a = TokenParser {
--          runTokenParser :: [MetaToken] -> Either (LineNum, ColNum, String) ([MetaToken], a)
--    }
newtype Parser a = Parser { runParser :: [Token] -> Maybe ([Token], a) }


instance Functor Parser where
  -- fmap = (<$>)
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



-- | Data type to represent the WhileAST Abstract Syntax Tree (AST).
data WhileAST = Assignment String Expression    --  Assignment of a variable with an expression
              | Sequential WhileAST WhileAST    --  Sequential composition of two statements
              | While Expression WhileAST       --  While loop with a condition and a body
              | Loop Expression WhileAST        --  Normal loop with a fix iteration number and a body
              | Skip                            --  A skip statement, representing no operation
              deriving (Show, Eq)

-- | Data type to represent expressions within WhileAST.
data Expression = Constant Int                    --  Integer constant
                | Variable String                 --  Variable reference
                | Add Expression Expression       --  Addition operation
                | Subtract Expression Expression  --  Subtraction operation
                | Neq Expression Expression       --  != operation
                deriving (Show, Eq)


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
  | c `elem` " \n\t" = tokenizeHelper cs ln -- Ignoriere Leerzeichen und Zeilenumbrüche
  | isAlpha c = uncurry keywordToken $ span (\x -> isAlpha x || isDigit x || x == '_') cx
  | isDigit c = uncurry (\x y -> (MetaToken ln 0 (ConstToken (read x))) : tokenizeHelper y ln) $ span isDigit cx
  | c == ':', Just rest <- stripPrefix "=" cs = MetaToken ln 0 AssignToken : tokenizeHelper rest ln
  | c == ';' = MetaToken ln 0 SemicolonToken : tokenizeHelper cs ln
  | c == '!', Just rest <- stripPrefix "=" cs = MetaToken ln 0 NotEqualToken : tokenizeHelper rest ln
  | c == '+' = MetaToken ln 0 PlusToken : tokenizeHelper cs ln
  | c == '-' = MetaToken ln 0 MinusToken : tokenizeHelper cs ln
  | otherwise = error $ "Ungültiges Zeichen: " ++ [c]
  where
    keywordToken :: String -> String -> [MetaToken]
    keywordToken token rest = case token of
      "loop"  -> MetaToken ln 0 LoopToken : tokenizeHelper rest ln
      "while" -> MetaToken ln 0 WhileToken : tokenizeHelper rest ln
      "do"    -> MetaToken ln 0 DoToken : tokenizeHelper rest ln
      "end"   -> MetaToken ln 0 EndToken : tokenizeHelper rest ln
      _       -> MetaToken ln 0 (VarToken token) : tokenizeHelper rest ln


tokenParser :: Token -> Parser Token
tokenParser t = Parser helper
  where
    -- helper :: [Token] -> Maybe ([Token], Token)
    helper [] = Nothing
    helper (x:xs)
      | x == t = Just (xs, t)
      | otherwise = Nothing


tokenListParser :: [Token] -> Parser [Token]
tokenListParser = sequenceA . map tokenParser


-- Read the content of a file and return it as a list of its lines with numbering
readFileContents :: FilePath -> IO [(LineNum, String)]
readFileContents filePath = readFile filePath >>= \c -> return $ zip [1..] (lines c)
