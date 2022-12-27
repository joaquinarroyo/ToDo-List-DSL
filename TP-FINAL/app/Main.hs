module Main (main) where

import Eval.EvalFilter(evalFilter)
import Structures.Task
import Structures.Folder
import Parser.Parser
import Control.Monad.Except
import Data.Maybe
import System.Console.Haskeline
import Data.Time
import Extra.Lib

main :: IO ()
main = runInputT defaultSettings (main' (newdir "root"))

main' :: Folder -> InputT IO ()
main' f = do s <- getInputLine "> "
             case s of
              Nothing -> return ()
              Just s -> do exp <- parseIO "<>" comm_parse s
                           outputStrLn (show exp)
                          --  case exp of
                          --    Nothing -> outputStrLn "error"
                          --    Just exp -> do t <- do d <-localTime "2020-01-01 00:00"
                          --                           newTask "tarea" "descripcion" 1 d
                          --                   v <- evalFilter t exp
                          --                   outputStrLn (show v)
                           main' f

parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case p x of
  Failed e -> do
    putStrLn (f ++ ": " ++ e)
    return Nothing
  Ok r -> return (Just r)

          
