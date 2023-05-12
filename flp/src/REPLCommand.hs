module REPLCommand where

import Lab2
import Control.Applicative (many, (<|>))

data REPLCommand
  = Quit
  | Load String
  | Eval String

parseQuit :: Parser REPLCommand
parseQuit = do
                reserved ":quit"
                return Quit

parseQ :: Parser REPLCommand
parseQ = do
              reserved ":q"
              return Quit

parseQUIT :: Parser REPLCommand
parseQUIT = do
                reserved ":Quit"
                return Quit

parseL :: Parser REPLCommand
parseL = do
                reserved ":l"
                text <- many anychar
                return (Load text)

parseLoad :: Parser REPLCommand
parseLoad = do
                reserved ":load"
                text <- many anychar
                return (Load text)
                
parseEval :: Parser REPLCommand
parseEval = do
                text <- many anychar
                return (Eval text)
                
replCommand :: Parser REPLCommand
replCommand = parseL <|> parseLoad <|> parseQ <|> parseQUIT <|> parseQuit <|> parseEval