module Main
  ( main
  ) where

import Command.AST
  ( Command(DeleteProfile, Exit, Help, LS, LoadProfile, Export, NewProfile,
        SaveProfile, ShowProfiles)
  )
import Command.Eval (eval)
import Control.Monad.Except (lift)
import Data.Map (toList)
import Export.Exporter (export)
import Extra.Pp (printError, printPrompt, showEnv, showTasks)
import Monad.Env (Env(..), getProfileName, getActualFolder, getRootFolder)
import Parser.Parser (ParseResult(Failed, Ok), comm_parse)
import Profile.Profile
  ( deleteProfile
  , firstLoad
  , lastProfileName
  , loadProfile
  , newProfile
  , saveProfile
  , showProfiles
  )
import Structures.Folder (newdir, tasks)
import Structures.Route (Route(..))
import System.Console.Haskeline
  ( InputT(..)
  , defaultSettings
  , getInputLine
  , outputStrLn
  , runInputT
  )

-- main
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      outputStrLn initialMessage
      -- Buscamos el ultimo perfil utilizado
      pn <- lift $ lastProfileName
      -- Si no existe, cargamos el perfil default
      -- Si existe, lo cargamos
      case pn of
        "" -> firstLoad' "default"
        _ -> firstLoad' pn
    firstLoad' pn = do
      f <- lift $ firstLoad pn
      case f of
        -- Si existe el perfil pn
        Just f -> main' (f, f, Empty, pn)
        -- Si se borro el perfil pn
        _ -> do
          lift $ newProfile pn
          main' (dir, dir, Empty, pn)
    dir = newdir "root"

-- Interprete
main' :: Env -> InputT IO ()
main' env = do
  input <- getInputLine $ printPrompt env
  case input of
    Nothing -> main' env
    Just "" -> main' env
    Just x -> do
      comm <- parseIO "Error" comm_parse x
      case comm of
        Just c -> handleCommand env c
        _ -> main' env

-- Maneja los comandos
handleCommand :: Env -> Command -> InputT IO ()
handleCommand env comm =
  case comm of
    Exit -> handleExit env
    Help -> do
      outputStrLn commands
      main' env
    LS -> do
      case showEnv env of
        "" -> main' env
        s -> do
          outputStrLn s
          main' env
    ----------- Comandos que utilizan archivos -----------
    NewProfile s -> do
      m <- lift $ newProfile s
      outputStrLn m
      main' env
    DeleteProfile -> do
      (b, m) <- lift $ deleteProfile pn
      outputStrLn m
      if b
        then handleCommand env (LoadProfile "default")
        else main' env
    SaveProfile -> do
      m <- lift $ saveProfile pn (getRootFolder env)
      outputStrLn m
      main' env
    LoadProfile s ->
      if s /= pn
        then do
          (f, m) <- lift $ loadProfile s
          outputStrLn m
          case f of
            Just f' -> do
              lift $ saveProfile pn (getRootFolder env)
              main' (f', f', Empty, s)
            _ -> main' env
        else main' env
    ShowProfiles -> do
      ps <- lift $ showProfiles
      outputStrLn ps
      main' env
    Export f -> do lift $ export f (map snd $ toList $ tasks $ getActualFolder env)
                   outputStrLn $ "Exported to " ++ show f
                   main' env
    ----------- Demas comandos -----------
    _ ->
      case eval comm env of
        Left e -> do
          outputStrLn $ printError e
          main' env
        Right (env', ts) ->
          case ts of
            [] -> main' env'
            _ -> do
              outputStrLn $ showTasks ts
              main' env'
  where
    pn = getProfileName env

-- Maneja el comando Exit
handleExit :: Env -> InputT IO ()
handleExit env = do
  input <- getInputLine $ "Do you want to save " ++ pn ++ " profile? (y/n) "
  case input of
    Just i ->
      case i of
        "y" -> do
          lift $ saveProfile pn (getRootFolder env)
          bye
        "n" -> bye
        _ -> handleExit env
    _ -> handleExit env
  where
    bye = outputStrLn "Bye!"
    pn = getProfileName env

-- Funcion robada de TPs de ALP
parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO f p x =
  lift $
  case p x of
    Failed e -> do
      putStrLn (f ++ ": " ++ e)
      return Nothing
    Ok r -> return (Just r)

-- Mensaje inicial, Comandos
initialMessage, commands :: String
initialMessage =
  "Lenguaje de organización de tareas\n\
    \Escriba help para recibir ayuda"
commands =
  "Comandos disponibles: \n\
    \  ls: lista las tareas/carpetas de la carpeta actual\n\
    \  cd <ruta>: cambia la carpeta actual a la ruta recibida\n\
    \  newdir <nombre>: crea una carpeta con el nombre recibido\n\
    \  newtask (<nombre>, <descripcion>, <prioridad - opcional>, <fecha - opcional>): crea una tarea con los datos recibidos\n\
    \  editdir <nombre>: edita la carpeta con el nombre recibido\n\
    \  edittask <nombre> set <campo> <valor>: edita el campo de la tarea con el nombre recibido\n\
    \  deletedir <nombre>: borra la carpeta con el nombre recibido\n\
    \  deletetask <nombre>: borra la tarea con el nombre recibido\n\
    \  complete <nombre>: completa la tarea con el nombre recibido\n\
    \  search [-r] <filter>: busca las tareas/carpetas que cumplan con el filtro recibido\n\
    \        para conocer la sintaxis del lenguaje de filtros, revise la documentación\n\
    \        -r indica que la busqueda se realiza recursivamente\n\
    \  export <fileType>: exporta las tareas de la carpeta actual a un archivo .fileType\n\
    \        para saber los tipos de archivos disponibles, revise la documentación\n\
    \  newprofile <nombre>: crea un nuevo perfil con el nombre recibido\n\
    \  deleteprofile: elimina el perfil actual\n\
    \  showprofiles: muestra los perfiles creados\n\
    \  load <nombre>: carga el perfil con el nombre recibido\n\
    \  save: guarda el perfil actual\n\
    \  exit: cierra el programa\n\
    \  help: muestra los comandos disponibles"
