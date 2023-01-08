module Main (main) where

import Control.Monad.Except (lift)
import Command.AST (Command (Exit, Help, LS))
import Eval.EvalCommand (eval)
import Extra.Pp (printPrompt, printError, showTasks, showEnv)
import Parser.Parser (ParseResult (Ok, Failed), comm_parse)
import Structures.Env (Env (..))
import Structures.Folder (newdir)
import Structures.Route (Route (..))
import System.Console.Haskeline (InputT (..), runInputT, getInputLine, outputStrLn, defaultSettings)

-- main
main :: IO ()
main = runInputT defaultSettings (main' env)
  where env = (dir, dir, Empty)
        dir = newdir "root"

-- Interprete
main' :: Env -> InputT IO ()
main' env = do input <- getInputLine $ printPrompt env
               case input of
                Nothing -> return ()
                Just x -> do comm <- parseIO "Error" comm_parse x
                             handleCommand env comm

-- Maneja los comandos
handleCommand :: Env -> Maybe Command -> InputT IO ()
handleCommand env comm = 
  case comm of
    Nothing -> main' env
    Just Exit -> do outputStrLn "Bye!"
                    return ()
    Just Help -> do outputStrLn showCommands
                    main' env
    Just LS -> do outputStrLn $ showEnv env
                  main' env
    Just c -> case eval c env of
                Left e -> do outputStrLn $ printError e
                             main' env
                Right (env', ts) -> case ts of
                                      [] -> main' env'
                                      _ -> do outputStrLn $ showTasks ts
                                              main' env'
                 

-- Funcion robada de TPs de ALP
parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case p x of
  Failed e -> do
    putStrLn (f ++ ": " ++ e)
    return Nothing
  Ok r -> return (Just r)

-- Muestra los comandos
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


          
