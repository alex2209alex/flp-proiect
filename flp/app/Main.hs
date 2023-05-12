module Main where

import System.IO

import Lab2
import Exp
import Parsing
import Printing
import REPLCommand
import GHC.IO.Handle (hFlush)
import Eval
import Sugar

main :: IO ()
main = do
    putStr "main>"
    hFlush stdout
    s <- getLine
    let comanda = parseFirst replCommand s
    case comanda of
        Just Quit -> return()
        Just (Load s) -> putStrLn s >> main
        Just (Eval s) -> case rezultat of
            Nothing -> putStrLn "Eroare"
            Just exp -> putStrLn $ showExp (sugarExp (normalize (desugarExp exp))) 
            where
            rezultat = parseFirst exprParser s
