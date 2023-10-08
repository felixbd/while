-- | Parser for the while programming language in haskell - (c) 2023 Felix Drees

module WhileParser
  (Token(..)
  ,WhileAST(..)
  ,Expression(..)
  ,tokenize
  ) where


import Data.Char (isDigit, isAlpha)
import Data.List (stripPrefix)
import Control.Applicative


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
type ColNum = Int
type MetaToken = (LineNum, ColNum, Token)

-- TODO(felix): upgrade current verwion to handel errors better
--  newtype TokenParser a = TokenParser {
--          runTokenParser :: [MetaToken] -> Either (LineNum, ColNum, String) ([MetaToken], a)
--    }
newtype Parser a = Parser { runParser :: [Token] -> Maybe ([Token], a) }

{-
instance Functor Parser where
  fmap func (Parser tokenList) = Parser helper
    where
      helper
-}

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
  (Parser pl) <|> (Parser pr) = Parser $ \x -> pl x <|> pr x



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
tokenize :: String -> [Token]
tokenize [] = []
tokenize cx@(c:cs)
  | c `elem` " \n\t" = tokenize cs -- Ignoriere Leerzeichen und Zeilenumbrüche
  | isAlpha c = uncurry keywordToken $ span (\x -> isAlpha x || isDigit x || x == '_') cx
  | isDigit c = uncurry (\x y -> ConstToken (read x) : tokenize y) $ span isDigit cx
  | c == ':', Just rest <- stripPrefix "=" cs = AssignToken : tokenize rest
  | c == ';' = SemicolonToken : tokenize cs
  | c == '!', Just rest <- stripPrefix "=" cs = NotEqualToken : tokenize rest
  | c == '+' = PlusToken : tokenize cs
  | c == '-' = MinusToken : tokenize cs
  | otherwise = error $ "Ungültiges Zeichen: " ++ [c]
  where
    keywordToken :: String -> String -> [Token]
    keywordToken token rest = case token of
      "loop"  -> LoopToken : tokenize rest
      "while" -> WhileToken : tokenize rest
      "do"    -> DoToken : tokenize rest
      "end"   -> EndToken : tokenize rest
      _       -> VarToken token : tokenize rest


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
