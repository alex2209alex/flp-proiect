module Main where

import System.IO

import Lab2
import Exp
import Parsing
import Printing
import REPLCommand
import Eval
import Sugar
import GHC.IO.Handle (hFlush)
import Program
import qualified Data.Map.Strict as Map

main :: IO ()
main = execute Map.empty

-- git checkout -b multi-arg

-- git add -u
-- git commit -m "Add multi-arg functionality."

-- git checkout main
-- git archive --output=./multi_arg.zip --format=zip multi-arg

execute env 
    = do
        putStr "miniHaskell>"
        hFlush stdout
        s <- getLine
        let comanda = parseFirst replCommand s
        case comanda of
            Nothing -> putStrLn "Cannot parse comand" >> execute env
            Just Quit -> return()
            Just (Load file) -> do
                result <- parseFromFile program file
                case result of
                    Left e  -> print e >> execute env
                    Right pgm -> execute (programEnv pgm)
            Just (Eval es) ->
                case parseFirst exprParser es of
                Nothing -> putStrLn "Error: cannot parse expression" >> execute env
                Just e ->
                    let simpleE = desugarExp e
                        simpleE' = normalizeEnv env simpleE
                        e' = sugarExp simpleE'
                    in putStrLn (showExp e') >> execute env
