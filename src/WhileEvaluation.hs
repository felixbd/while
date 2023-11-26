-- | Evaluation of a while AST in haskell - (c) 2023 Felix Drees - BSD3 License

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module WhileEvaluation
  ( -- eval
  ) where


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
newtype VarStateM a = VarStateM { getVST :: VarStateT a } deriving Functor

instance Applicative VarStateM where
  pure x = VarStateM (x,)

  vstf <*> vst = VarStateM (getVST vstf
                            >>>= \f -> getVST vst
                            >>>= \x -> getVST (pure (f x)))

instance Monad VarStateM where
  vst >>= f = VarStateM (getVST vst >>>= getVST . f)



lookUpVarState :: VarName -> VarStateWorld -> Int
lookUpVarState varName [] = error $ "\n\ESC[91m[REFERENCED BEFORE ASSIGNMENT]\ESC[0m " ++ varName
lookUpVarState varName ((name, value):xs) | varName == name = value
                                          | otherwise       = lookUpVarState varName xs

updateVarState :: VarName -> Int -> VarStateWorld -> VarStateWorld
updateVarState name val states | not (name `elem` (map fst states)) = (name, val):states
                               | otherwise = [(varName, if name == varName then val else varVal)
                                             | (varName, varVal) <- states]

--------------------------------------------------------------------------------

evalExpression :: Expression -> VarStateWorld -> Int
evalExpression exp state = case exp of
                             (Constant c)         -> c
                             (Variable varName)   -> lookUpVarState varName state
                             (Add exp1 exp2)      -> helper (+) [exp1, exp2]
                             (Subtract exp1 exp2) -> helper (-) [exp1, exp2]
                             (Neq exp1 exp2)      -> fromEnum $ (evalExpression exp1 state) /= (evalExpression exp2 state)
                             _                    -> undefined
  where
    -- helper :: (Int -> Int -> Int) -> [Expression] -> Int
    helper f es = foldl (\acc x -> f acc (evalExpression x state)) 0 es

--------------------------------------------------------------------------------

evalAssignment :: WhileAST -> VarStateWorld -> VarStateWorld
evalAssignment (Assignment name exp) state = updateVarState name (evalExpression exp state) state
evalAssignment _ _ = undefined

evalWhileExp :: WhileAST -> VarStateWorld -> VarStateWorld
evalWhileExp (While exp whileAST) state = undefined  -- TODO ...
evalWhileExp _ _ = undefined

evalLoopExp :: WhileAST -> VarStateWorld -> VarStateWorld
-- evalLoopExp (Loop exp whileAST) state = (foldr (.) id (replicate (evalExpression exp state) eval))
evalLoopExp (Loop exp whileAST) state = helperLoop whileAST state (evalExpression exp state)
evalLoopExp _ _ = undefined

helperLoop :: WhileAST -> VarStateWorld -> Int -> VarStateWorld
helperLoop ast state index = if index > 0
                             then helperLoop ast (eval ast state) (index - 1)
                             else state

--------------------------------------------------------------------------------

eval :: WhileAST -> VarStateWorld -> VarStateWorld
eval ast vs = vs  -- TODO ...

evalT :: WhileAST -> VarStateT ()
evalT ast w = ((), eval ast w)

evalM :: WhileAST -> VarStateM ()
evalM = VarStateM . evalT
