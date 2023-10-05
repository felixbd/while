-- | Parser for the while programming language in haskell - (c) 2023 Felix Drees

module WhileParser
  (Token(..)
  ,tokenize
  ) where


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

