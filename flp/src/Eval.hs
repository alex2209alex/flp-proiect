module Eval where

import Exp
import Data.List ( union, delete, nub )
import Prelude hiding (exp)

distincte :: Eq a => [a] -> [a]
distincte [] = []
distincte (x:xs)   | x `elem` xs   = distincte xs
                | otherwise     = x : distincte xs

vars :: Exp -> [IndexedVar]
vars (X x) = [x]
vars (Lam x y) = distincte (x : vars y)
vars (App x y) = distincte (vars x ++ vars y)

-- >>> vars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0}]

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

freeVars :: Exp -> [IndexedVar]
freeVars (X x) = [x]
freeVars (Lam x y) = removeItem x (vars y)
freeVars (App x y) = distincte (freeVars x ++ freeVars y)

-- >>> freeVars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "y", ivCount = 0}]

occursFree :: IndexedVar -> Exp -> Bool
occursFree x exp = x `elem` freeVars exp

-- >>> makeIndexedVar "x" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- False

-- >>> makeIndexedVar "y" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- True

freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar (IndexedVar name count) xs =
    if IndexedVar name count `elem` xs then
        freshVar (IndexedVar name (count + 1)) xs
    else
        IndexedVar name count

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x"]
-- IndexedVar {ivName = "x", ivCount = 1}

renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement (X x) = 
    if x == toReplace then X replacement
    else (X x)
renameVar toReplace replacement (Lam x exp) = 
    if x == toReplace then Lam replacement (renameVar toReplace replacement exp)
    else Lam x (renameVar toReplace replacement exp)
renameVar toReplace replacement (App exp exp2) = App (renameVar toReplace replacement exp) (renameVar toReplace replacement exp2)
        

substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement exp = renameVar toReplace (freshVar toReplace (vars exp)) exp

normalize :: Exp -> Exp
normalize (X x) = (X x)
normalize (App m n) = App (normalize m) (normalize n)
normalize (Lam x (App m n)) = normalize (substitute x n m)
normalize (Lam x m) = Lam x (normalize m)

-- >>> normalize (X (makeIndexedVar "x"))
-- X (IndexedVar {ivName = "x", ivCount = 0})

-- >>> normalize (Lam (makeIndexedVar "x") (X (makeIndexedVar "x")))

-- >>> normalize (Lam (makeIndexedVar "x") (App (X (makeIndexedVar "y")) (X (makeIndexedVar "y"))))