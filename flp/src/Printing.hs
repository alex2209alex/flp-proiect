module Printing (showExp) where
import Data.List (intercalate)

import Exp

showVar :: Var -> String
showVar = getVar

inParens :: String -> String
inParens s = "(" ++ s ++ ")"

eCLam :: ComplexExp -> Bool
eCLam (CLam x e) = True
eCLam _ = False

showCLam :: ComplexExp -> String
showCLam (CLam x e) = showVar x ++ " " ++ showCLam e
showCLam e = "-> " ++ showExp e

showExp :: ComplexExp -> String
showExp (CX x) = showVar x
showExp (Nat n) = show n
showExp (CLam x e) = if eCLam e then
                        inParens ("\\" ++ showVar x ++ " " ++ showCLam e)
                    else
                        inParens ("\\" ++ showVar x ++ " -> " ++ showExp e)
showExp (CApp e1 e2) = inParens (showExp e1 ++ " " ++ showExp e2)
showExp (Let x ex e) = inParens ("let " ++ showVar x ++ " := " ++ showExp ex ++ " in " ++ showExp e)
showExp (LetRec x ex e) = inParens ("letrec " ++ showVar x ++ " := " ++ showExp ex ++ " in " ++ showExp e)
showExp (List l) = "[" ++ intercalate "," (map showExp l) ++ "]"
