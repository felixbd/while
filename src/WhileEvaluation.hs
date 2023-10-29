-- | Evaluation of a while AST in haskell - (c) 2023 Felix Drees - BSD3 License

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

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

---

-- | Variable State Transformer
type VarStateT a = VarStateWorld -> (a, VarStateWorld)

infixl 1 >>>=
(>>>=) :: VarStateT a -> (a -> VarStateT b) -> VarStateT b
vst >>>= f = uncurry f . vst

-- | Variable State Monade
newtype VarStateM a = VarStateM { getVST :: VarStateT a } deriving Functor

instance Applicative VarStateM where
  pure x = VarStateM (x,)

  vstf <*> vst = VarStateM (getVST vstf >>>=
                            \f -> getVST vst >>>=
                            \x -> getVST (pure (f x)))

instance Monad VarStateM where
  vst >>= f = VarStateM (getVST vst >>>= getVST . f)



lookUpVarState :: VarName -> VarStateWorld -> Int
lookUpVarState varName [] = error $ "\n\ESC[91m[REFERENCED BEFORE ASSIGNMENT]\ESC[0m " ++ varName
lookUpVarState varName ((n, v):xs) | varName == n = v
                                   | otherwise    = lookUpVarState varName xs


-- evalAssignment :: (Assignment String Expression) -> VarStateWorld -> VarStateWorld
-- evalAssignment a v = v

{-
eval :: WhileAST -> VarStateWorld -> VarStateWorld
eval ast vs = vs

evalT :: WhileAST -> VarStateT ()
evalT ast w = ((), eval ast w)

evalM :: WhileAST -> VarStateM ()
evalM = VarStateM . evalT
-}
