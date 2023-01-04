module Main (main) where

import Eval.EvalCommand
import Structures.Task
import Structures.Folder
import Structures.Env
import Structures.Route
import Extra.Pp
import Command.AST
import Parser.Parser
import Control.Monad.Except
import System.Console.Haskeline

-- import parse
import Text.ParserCombinators.Parsec

main :: IO ()
main = runInputT defaultSettings (main' env)
  where env = (dir, dir, Empty)
        dir = newdir "root"

-- Interprete
main' :: Env -> InputT IO ()
main' env = do input <- getInputLine (prompt env)
               case input of
                Nothing -> return ()
                Just x -> do comm <- parseIO "Error" comm_parse x
                             case comm of
                              Nothing -> main' env
                              Just Exit -> do outputStrLn "Bye!"
                                              return ()
                              Just Help -> do outputStrLn showCommands
                                              main' env
                              Just LS -> do outputStrLn (showEnv env)
                                            main' env
                              Just c -> do let r = eval c env
                                           case r of
                                            Left e -> do outputStrLn (printError e)
                                                         main' env
                                            Right (env', ts) -> 
                                              if ts /= []
                                              then do outputStrLn (showTasks ts)
                                                      main' env'
                                              else main' env'

parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case p x of
  Failed e -> do
    putStrLn (f ++ ": " ++ e)
    return Nothing
  Ok r -> return (Just r)

prompt :: Env -> String
prompt env = printPrompt ("~/" ++ showRoute env ++ "$ ")

showCommands :: String
showCommands = "Comandos disponibles: \n\
               \  ls: lista las tareas/carpetas de la carpeta actual\n\
               \  cd <ruta>: cambia la carpeta actual a la ruta recibida\n\
               \  newdir <nombre>: crea una carpeta con el nombre recibido\n\
               \  newtask (<nombre>, <descripcion>, <prioridad - opcional>, <fecha - opcional>): crea una tarea con los datos recibidos\n\
               \  editdir <nombre>: edita la carpeta con el nombre recibido\n\
               \  edittask <nombre> <campo> <valor>: edita el campo de la tarea con el nombre recibido\n\
               \  deletedir <nombre>: borra la carpeta con el nombre recibido\n\
               \  deletetask <nombre>: borra la tarea con el nombre recibido\n\
               \  complete <nombre>: completa la tarea con el nombre recibido\n\
               \  exit: cierra el programa\n\
               \  help: muestra los comandos disponibles"


          
