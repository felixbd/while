-- | Evaluation of a while AST in haskell - (c) 2023 Felix Drees - BSD3 License

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module WhileEvaluation
  (eval
  ,calcInitialVarStates
  ,isSimpleArithmeticExpression
  ) where


import WhileParser
  (Token(..)
  ,WhileAST(..)
  ,Expression(..)
  ,tokenize
  ,readFileContent
  ,mainParser
  ,runParser
  ,many
  )


type VariableState = (String, Maybe Int)
type VariableStates = [VariableState]

type VariableStatesTransformer a = VariableStates -> (a, VariableStates)

infixl 1 >>>=
(>>>=) :: VariableStatesTransformer a -> (a -> VariableStatesTransformer b) -> VariableStatesTransformer b
varStateTransformer >>>= f = uncurry f . varStateTransformer

newtype VariableStatesMonad a = VariableStatesMonad { asT :: VariableStatesTransformer a } deriving Functor

instance Applicative VariableStatesMonad where
  pure x = VariableStatesMonad (x,)
  vstf <*> vst = VariableStatesMonad (asT vstf >>>= \f -> asT vst >>>= \x -> asT (pure (f x)))

instance Monad VariableStatesMonad where
  vst >>= f = VariableStatesMonad (asT vst >>>= asT . f)


-- calc initial world state (relevant for monad) ------------------------------

calcInitialVarStates :: WhileAST -> [VariableState]
calcInitialVarStates Pass = []
calcInitialVarStates (While _ ast) = calcInitialVarStates ast
calcInitialVarStates (Loop _ ast) = calcInitialVarStates ast
calcInitialVarStates (Assignment varName expr) = [(varName, evaluateSimple expr)]
calcInitialVarStates (Sequential ast1 ast2) = calcInitialVarStates ast1
                                              ++ calcInitialVarStates ast2

evalExp :: String -> Expression -> VariableState
evalExp = (. evaluateSimple) . (,)

{-|
   These functions work together to evaluate simple arithmetic expressions, ensuring
   that the result is a non-negative integer. The 'evaluateSimple' function takes
   an 'Expression' and returns 'Maybe Int' where 'Nothing' indicates an invalid
   or non-simple expression. It leverages the 'isSimpleArithmeticExpression' and
   'evaluateMath' functions to perform the calculations.

   - 'evaluateSimple' evaluates the expression and ensures the result is non-negative.
   - 'evaluateMath' recursively computes the value of the expression from right to left,
     as addition and subtraction are associative operations in Z (integers) but not in N (natural numbers).
     The right-to-left evaluation is required due to the recursive nature of the calculations.
     The result is then checked for validity in N.
   - 'isSimpleArithmeticExpression' checks if an expression is a simple arithmetic expression,
     defined as a combination of constants, additions, and subtractions.
-}

evaluateSimple :: Expression -> Maybe Int
evaluateSimple e = max 0 <$>
                   if isSimpleArithmeticExpression e
                   then Just (evaluateMath e)
                   else Nothing

evaluateMath :: Expression -> Int
evaluateMath (Constant n) = n
evaluateMath (Add e1 e2) = evaluateMath e1 + evaluateMath e2
evaluateMath (Subtract e1 e2) = evaluateMath e1 - evaluateMath e2
evaluateMath _ = undefined

isSimpleArithmeticExpression :: Expression -> Bool
isSimpleArithmeticExpression (Constant _)     = True
isSimpleArithmeticExpression (Add e1 e2)      = isSimpleArithmeticExpression e1
                                                 && isSimpleArithmeticExpression e2
isSimpleArithmeticExpression (Subtract e1 e2) = isSimpleArithmeticExpression e1
                                                 && isSimpleArithmeticExpression e2
isSimpleArithmeticExpression _                = False

-- eval while ast -------------------------------------------------------------


-- TODO(all): ...

eval :: WhileAST-> VariableStates -> VariableStates
eval ast vs = vs

evalT :: WhileAST -> VariableStatesTransformer ()
evalT ast w = ((), eval ast w)

evalM :: WhileAST -> VariableStatesMonad ()
evalM = VariableStatesMonad . evalT
