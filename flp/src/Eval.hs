module Eval where

import Exp
import Data.List ( union, delete, nub )
import Prelude hiding (exp)
import Sugar (desugarExp)

vars :: Exp -> [IndexedVar]
vars (X x) = [x]
vars (App e1 e2) = vars e1 `union` vars e2
vars (Lam x e) = [x] `union` vars e

-- >>> vars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0}]

freeVars :: Exp -> [IndexedVar]
freeVars (X x) = [x]
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Lam x e) = delete x (freeVars e)

-- >>> freeVars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "y", ivCount = 0}]

occursFree :: IndexedVar -> Exp -> Bool
occursFree x exp = x `elem` freeVars exp

-- >>> makeIndexedVar "x" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- False

-- >>> makeIndexedVar "y" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- True

freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar x xs = x {ivCount = m + 1}
   where  
      nxs = [ivCount y | y <- x : xs, ivName y == ivName x]
      m = maximum nxs

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x"]
-- IndexedVar {ivName = "x", ivCount = 1}

renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement = go
  where
    go (X x)
      = X (if x == toReplace then replacement else x)
    go (App e1 e2) = App (go e1) (go e2)
    go (Lam x e)
      = Lam (if x == toReplace then replacement else x) (go e)
        

substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement = go
  where
    go (X x)
      | x == toReplace = replacement
      | otherwise = X x
    go (App e1 e2) = App (go e1) (go e2)
    go (Lam x e)
      | x == toReplace = Lam x e
      | x `occursFree` replacement =
          let f = freshVar x (vars e `union` vars replacement)
           in Lam f (go (renameVar x f e))
      | otherwise = Lam x (go e)
      
step :: Exp -> Maybe Exp
step (X x) = Nothing
step (Lam x m) = fmap (Lam x) (step m)
step (App (Lam x m) n) = Just (substitute x n m)
step (App m n) = case step m of
                    Nothing -> case step n of
                                Nothing -> Nothing
                                Just x -> Just (App m x)
                    Just x -> Just (App x n)

normalize :: Exp -> Exp
normalize exp = maybe exp normalize (step exp)

-- >>> normalize (X (makeIndexedVar "x"))
-- X (IndexedVar {ivName = "x", ivCount = 0})

-- >>> normalize (Lam (makeIndexedVar "x") (X (makeIndexedVar "x")))

-- >>> normalize (Lam (makeIndexedVar "x") (App (X (makeIndexedVar "y")) (X (makeIndexedVar "y"))))
-- X (IndexedVar {ivName = "y", ivCount = 0})