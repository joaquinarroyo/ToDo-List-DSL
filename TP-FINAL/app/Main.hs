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
import Monad.Env (Env (..), getActualFolder, getProfileName)
import Profile.Profile (firstLoad, 
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
        firstLoad' pn = do f <- lift $ firstLoad pn
                           case f of
                            -- Si existe el perfil pn
                            Just f -> main' (f, f, Empty, pn)
                            -- Si se borro el perfil pn
                            _ -> do lift $ newProfile pn
                                    main' (dir, dir, Empty, pn)
        dir = newdir "root"

-- Interprete
main' :: Env -> InputT IO ()
main' env = do input <- getInputLine $ printPrompt env
               case input of
                Nothing -> main' env
                Just "" -> main' env
                Just x -> do comm <- parseIO "Error" comm_parse x
                             case comm of
                              Just c -> handleCommand env c
                              _ -> main' env

-- Maneja los comandos
handleCommand :: Env -> Command -> InputT IO ()
handleCommand env comm = 
  case comm of
    Exit -> handleExit env
    Help -> do outputStrLn commands
               main' env
    LS -> do case showEnv env of
               "" -> main' env
               s -> do outputStrLn s
                       main' env
    NewProfile s -> do lift $ newProfile s
                       main' env
    DeleteProfile -> do b <- lift $ deleteProfile pn
                        if b
                        then handleCommand env (LoadProfile "default")
                        else main' env
    SaveProfile -> do lift $ saveProfile pn (getActualFolder env)
                      main' env
    LoadProfile s -> if s /= pn
                      then do f <- lift $ loadProfile s
                              case f of
                                Just f' -> do lift $ saveProfile pn (getActualFolder env)
                                              main' (f', f', Empty, s)
                                _ -> main' env
                      else main' env
    _ -> case eval comm env of
          Left e -> do outputStrLn $ printError e
                       main' env
          Right (env', ts) -> case ts of
                              [] -> main' env'
                              _ -> do outputStrLn $ showTasks ts
                                      main' env'
    where pn = getProfileName env
                 
-- Maneja el comando Exit
handleExit :: Env -> InputT IO ()
handleExit env = do input <- getInputLine $ "Do you want to save " ++ pn ++ " profile? (y/n) "
                    case input of
                      Just i -> case i of
                                 "y" -> do lift $ saveProfile pn (getActualFolder env)
                                           bye
                                 "n" -> bye
                                 _ -> handleExit env
                      _ -> handleExit env
    where bye = outputStrLn "Bye!"
          pn = getProfileName env

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


          
