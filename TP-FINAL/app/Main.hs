module Main (main) where

import Control.Monad.Except (lift)
import Command.AST (Command ( Exit, 
                              Help, 
                              LS, 
                              NewProfile, 
                              DeleteProfile,
                              SaveProfile, 
                              LoadProfile))
import Command.Eval (eval)
import Extra.Pp (printPrompt, 
                 printError, 
                 showTasks, 
                 showEnv)
import Parser.Parser (ParseResult (Ok, Failed), comm_parse)
import Monad.Env (Env (..))
import Profile.Profile (ProfileName, firstLoad, 
                                     newProfile, 
                                     deleteProfile, 
                                     saveProfile, 
                                     loadProfile, 
                                     lastProfileName)
import Structures.Folder (newdir)
import Structures.Route (Route (..))
import System.Console.Haskeline (InputT (..), runInputT, 
                                              getInputLine, 
                                              outputStrLn, 
                                              defaultSettings)

-- main
main :: IO ()
main = runInputT defaultSettings loop 
    where 
        loop = do pn <- lift $ lastProfileName
                  case pn of
                    "" -> firstLoad' "default"
                    _ -> firstLoad' pn
        firstLoad' pn = do f <- firstLoad pn
                           case f of
                            Just f -> main' (f, f, Empty) pn
                            _ -> do newProfile pn
                                    main' (dir, dir, Empty) pn
        dir = newdir "root"

-- Interprete
main' :: Env -> ProfileName -> InputT IO ()
main' env pn = do input <- getInputLine $ printPrompt env pn
                  case input of
                    Nothing -> main' env pn
                    Just "" -> main' env pn
                    Just x -> do comm <- parseIO "Error" comm_parse x
                                 case comm of
                                  Just c -> handleCommand env pn c
                                  _ -> main' env pn

-- Maneja los comandos
handleCommand :: Env -> ProfileName -> Command -> InputT IO ()
handleCommand env pn comm = 
  case comm of
    Exit -> handleExit env pn
    Help -> do outputStrLn commands
               main' env pn
    LS -> do case showEnv env of
               "" -> main' env pn
               s -> do outputStrLn s
                       main' env pn
    NewProfile s -> do newProfile s
                       main' env pn
    DeleteProfile -> do b <- deleteProfile pn
                        if b
                        then handleCommand env pn (LoadProfile "default")
                        else main' env pn
    SaveProfile -> do saveProfile pn env
                      main' env pn
    LoadProfile s -> if s /= pn
                      then do f <- loadProfile s
                              case f of
                                Just f' -> do saveProfile pn env
                                              main' (f', f', Empty) s
                                _ -> main' env pn
                      else main' env pn
    _ -> case eval comm env of
          Left e -> do outputStrLn $ printError e
                       main' env pn
          Right (env', ts) -> case ts of
                              [] -> main' env' pn
                              _ -> do outputStrLn $ showTasks ts
                                      main' env' pn
                 
-- Maneja el comando Exit
handleExit :: Env -> ProfileName -> InputT IO ()
handleExit env pn = do input <- getInputLine $ "Do you want to save " ++ pn ++ " profile? (y/n) "
                       case input of
                          Just i -> case i of
                                      "y" -> do saveProfile pn env
                                                bye
                                      "n" -> bye
                                      _ -> handleExit env pn
                          _ -> handleExit env pn
    where bye = outputStrLn "Bye!"

-- Funcion robada de TPs de ALP
parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case p x of
  Failed e -> do
    putStrLn (f ++ ": " ++ e)
    return Nothing
  Ok r -> return (Just r)


-- Mensaje inicial, Comandos
initialMessage, commands :: String
initialMessage = "Lenguaje de organización de tareas\n\
                  \Escriba help para recibir ayuda \n"
commands = "Comandos disponibles: \n\
               \  ls: lista las tareas/carpetas de la carpeta actual\n\
               \  cd <ruta>: cambia la carpeta actual a la ruta recibida\n\
               \  newdir <nombre>: crea una carpeta con el nombre recibido\n\
               \  newtask (<nombre>, <descripcion>, <prioridad - opcional>, <fecha - opcional>): crea una tarea con los datos recibidos\n\
               \  editdir <nombre>: edita la carpeta con el nombre recibido\n\
               \  edittask <nombre> <campo> <valor>: edita el campo de la tarea con el nombre recibido\n\
               \  deletedir <nombre>: borra la carpeta con el nombre recibido\n\
               \  deletetask <nombre>: borra la tarea con el nombre recibido\n\
               \  complete <nombre>: completa la tarea con el nombre recibido\n\
               \  newprofile <nombre>: crea un nuevo perfil con el nombre recibido\n\
               \  deleteprofile: elimina el perfil actual\n\
               \  load <nombre>: carga el perfil con el nombre recibido\n\
               \  save: guarda el perfil actual\n\
               \  exit: cierra el programa\n\
               \  help: muestra los comandos disponibles"


          
