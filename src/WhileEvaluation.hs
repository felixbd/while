-- | Evaluation of a while AST in haskell
-- | (c) 2023 Felix Drees - BSD3 License

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module WhileEvaluation
  (evalM
  ,evalT
  ,eval
  ,VarName
  ,VarVal
  ,VarState
  ,VarStateWorld
  ,VarStateM(..)
  ,runEvalM
  ) where

import Data.List (nub)

import WhileParser
  (WhileAST(..)
  ,Expression(..)
  )


type VarName       = String
type VarVal        = Int
type VarState      = (VarName, VarVal)
type VarStateWorld = [VarState]


-- | Variable State Transformer
type VarStateT a = VarStateWorld -> (a, VarStateWorld)


infixl 1 >>>=
(>>>=) :: VarStateT a -> (a -> VarStateT b) -> VarStateT b
vst >>>= f = uncurry f . vst

-- | Variable State Monade
newtype VarStateM a = VarStateM { getVarState :: VarStateT a } deriving Functor

instance Applicative VarStateM where
  pure x = VarStateM (x,)

  vstf <*> vst = VarStateM (getVarState vstf
                            >>>= \f -> getVarState vst
                            >>>= \x -> getVarState (pure (f x)))

instance Monad VarStateM where
  vst >>= f = VarStateM (getVarState vst >>>= getVarState . f)


-- getState :: VarStateM VarStateWorld
-- getState = VarStateM (\s -> (s, s))

-- putState :: VarStateWorld -> VarStateM ()
-- putState s = VarStateM (const ((), s))

--------------------------------------------------------------------------------

lookUpVarState :: VarName -> VarStateWorld -> Int
lookUpVarState varName [] = error $ "\n\ESC[91m[REFERENCED BEFORE ASSIGNMENT]\ESC[0m " ++ varName
lookUpVarState varName ((name, value):xs) | varName == name = value
                                          | otherwise       = lookUpVarState varName xs

updateVarState :: VarName -> Int -> VarStateWorld -> VarStateWorld
updateVarState name val states | name `notElem` map fst states = (name, val):states
                               | otherwise = [(varName, if name == varName then val else varVal)
                                             | (varName, varVal) <- states]

--------------------------------------------------------------------------------

evalExpression :: Expression -> VarStateWorld -> Int
evalExpression expr state = case expr of
                             (Constant c)         -> c
                             (Variable varName)   -> lookUpVarState varName state
                             (Add exp1 exp2)      -> foldl (\acc x -> acc + evalExpression x state) 0 [exp1, exp2]
                             (Subtract exp1 exp2) -> max 0 $ evalExpression exp1 state - evalExpression exp2 state
                             (Neq exp1 exp2)      -> fromEnum $ length (nub $ map (`evalExpression` state) [exp1, exp2]) == 2

--------------------------------------------------------------------------------

evalAssignment :: WhileAST -> VarStateWorld -> VarStateWorld
evalAssignment (Assignment name expr) state = updateVarState name (evalExpression expr state) state
evalAssignment _ _ = undefined

evalWhileExp :: WhileAST -> VarStateWorld -> VarStateWorld
evalWhileExp (While expr whileAST) state = helperWhile expr whileAST state 0
evalWhileExp _ _ = undefined

helperWhile :: Expression -> WhileAST -> VarStateWorld -> Int -> VarStateWorld
helperWhile p ast state maxRec
  | maxRec >= getRecursionLimit = error "\n\ESC[91m[RecursionError]\ESC[0m: maximum recursion depth exceeded!"
  | evalExpression p state == 1 = helperWhile p ast (eval ast state) (maxRec + 1)
  | otherwise = state
  where
    -- getRecursionLimit :: Int
    getRecursionLimit = 1500


evalLoopExp :: WhileAST -> VarStateWorld -> VarStateWorld
-- evalLoopExp (Loop exp whileAST) state = (foldr (.) id (replicate (evalExpression exp state) eval))
-- foldr (\_ accState -> evalLoopExp whileAST (eval expr accState)) state [1..evalExpression expr state]
evalLoopExp (Loop expr whileAST) state = helperLoop whileAST state (evalExpression expr state)
evalLoopExp _ _ = undefined

helperLoop :: WhileAST -> VarStateWorld -> Int -> VarStateWorld
helperLoop ast state index = if index > 0
                             then helperLoop ast (eval ast state) (index - 1)
                             else state

--------------------------------------------------------------------------------

eval :: WhileAST -> VarStateWorld -> VarStateWorld
eval ast vs = case ast of
                a@(Assignment _ _)      -> evalAssignment a vs
                (Sequential ast1 ast2)  -> eval ast2 $ eval ast1 vs
                w@(While _ _)           -> evalWhileExp w vs
                l@(Loop _ _)            -> evalLoopExp l vs
                Pass                    -> vs

evalT :: WhileAST -> VarStateT ()
evalT ast w = ((), eval ast w)

evalM :: WhileAST -> VarStateM ()
evalM = VarStateM . evalT


runEvalM :: WhileAST -> VarStateWorld -> ((), VarStateWorld)
runEvalM ast = getVarState $ evalM ast
