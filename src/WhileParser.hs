-- | Parser for the while programming language in haskell
-- | (c) 2023 Felix Drees - BSD3 License

{-# LANGUAGE LambdaCase #-}

module WhileParser
  (Token(..)
  ,WhileAST(..)
  ,Expression(..)
  ,tokenize
  ,readFileContent
  ,runASTParser
  ,mainParser
  ,runParser
  ,many
  ) where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isAlpha)
import Data.List (stripPrefix)

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

data WhileAST = Assignment String Expression    --  Assignment of a variable with an expression
              | Sequential WhileAST WhileAST    --  Sequential composition of two statements
              | While Expression WhileAST       --  While loop with a condition and a body
              | Loop Expression WhileAST        --  Normal loop with a fix iteration number and a body
              | Pass                            --  if there is no code after a semicolon
              deriving (Show, Eq)

data Expression = Constant { getConst :: Int }    --  Integer constant
                | Variable { getVar :: String }   --  Variable reference
                | Add Expression Expression       --  Addition operation
                | Subtract Expression Expression  --  Subtraction operation
                | Neq Expression Expression       --  != operation
                deriving (Show, Eq)

newtype Parser a = Parser { runParser :: [MetaToken] -> Maybe ([MetaToken], a) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ p >=> \(tks, x) -> Just (tks, f x)

instance Applicative Parser where
  pure x = Parser $ \xs -> Just (xs, x)

  -- <* :: f a -> f b -> f a
  -- *> :: f a -> f b -> f b
  (Parser pl) <*> (Parser pr) = Parser $ pl >=> \(ty, f) -> pr ty >>= \(tz, t) -> Just (tz, f t)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser pl) <|> (Parser pr) = Parser $ \x -> pl x <|> pr x  -- since Maybe implements Alternative ...


-- tokenizer ------------------------------------------------------------------

tokenize :: [(LineNum, String)] -> [MetaToken]
tokenize = concatMap $ uncurry (flip tokenizeHelper)
  where
    -- tokenizeHelper :: String -> LineNum -> [MetaToken]
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
      | otherwise = error $ "\n\ESC[91m[TOKENIZER ERROR]\ESC[0m Invalid char: -> "
                                ++ [c] ++ " <- in line: " ++ show ln ++ "\n"
        where
          -- keywordToken :: String -> String -> [MetaToken]
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

readFileContent :: FilePath -> IO [(LineNum, String)]
readFileContent filePath = putStrLn ("\n\ESC[92m[READING .WHILE FILE]\ESC[0m " ++ show filePath)
                                >> readFile filePath
                                >>= \c -> return $ zip [1..] (lines c)

-- begin parser expression ----------------------------------------------------

constIntParser :: Parser Expression
constIntParser = Constant . getConstInt . getToken <$> tokenParser (MetaToken 0 (ConstToken 0))

varNameParser :: Parser Expression
varNameParser = Variable . getVarName . getToken <$> tokenParser (MetaToken 0 (VarToken ""))

opExpParser :: MetaToken -> Parser Expression
opExpParser mt = f <$> ((varNameParser <|> constIntParser) <* tokenParser mt) <*> expressionParser
  where
    f = case getToken mt of
      PlusToken     -> Add
      MinusToken    -> Subtract
      NotEqualToken -> Neq
      -- this should never happer (this func will not be exportet anyway)
      _             -> error "\n\ESC[91m[INVALID OPERATION]\ESC[0m the given operation is not supported"

addExpressionParser :: Parser Expression
addExpressionParser = opExpParser $ MetaToken 0 PlusToken

subtractExpressionParser :: Parser Expression
subtractExpressionParser = opExpParser $ MetaToken 0 MinusToken

neqExpressionParser :: Parser Expression
neqExpressionParser = opExpParser $ MetaToken 0 NotEqualToken

expressionParser :: Parser Expression
expressionParser = addExpressionParser <|> subtractExpressionParser
                   <|> neqExpressionParser <|> constIntParser <|> varNameParser

-- begin parser WhileAST ----------------------------------------------------

assignmentParser :: Parser WhileAST
assignmentParser = Assignment . getVar <$> (varNameParser <* tokenParser (MetaToken 0 AssignToken))
                                        <*> expressionParser

whileParser :: Parser WhileAST
whileParser = While <$> (tokenParser (MetaToken 0 WhileToken)
                         *> expressionParser
                         <* tokenParser (MetaToken 0 DoToken))
                         <*> mainParser
                         <* tokenParser (MetaToken 0 EndToken)

loopParser :: Parser WhileAST
loopParser = Loop <$> (tokenParser (MetaToken 0 LoopToken)
                       *> expressionParser
                       <* tokenParser (MetaToken 0 DoToken))
                       <*> mainParser
                       <* tokenParser (MetaToken 0 EndToken)

sequenceParser :: Parser WhileAST
sequenceParser = Sequential <$> (assignmentParser <|> whileParser <|> loopParser)
                                <*> (tokenParser (MetaToken 0 SemicolonToken)
                                *> (mainParser <|> pure Pass))

mainParser :: Parser WhileAST
mainParser = sequenceParser <|> assignmentParser <|> whileParser <|> loopParser

fromMaybeParser :: Maybe ([MetaToken], [WhileAST]) -> WhileAST
fromMaybeParser x = if maybe False (null . fst) x
         then maybe Pass (head . snd) x
         else error ("\n\ESC[91m[PARSER ERROR]\ESC[0m faild to parse "
              ++ maybe "!" (show . head . fst) x)  -- >> Pass

runASTParser :: [MetaToken] -> WhileAST
runASTParser tokens = fromMaybeParser $ runParser (many mainParser) tokens
