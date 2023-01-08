module Extra.Pp
    (printError, 
     printPrompt,
     printHappyError,
     showEnv,
     showTasks
    ) 
    where

import Data.Map as M (null, toList)
import Data.List (sort)
import Extra.Error (Error (..) )
import Structures.Env (Env(..))
import Structures.Folder (Folder (..))
import Structures.Task (Task (..))
import Text.PrettyPrint.ANSI.Leijen

-- Renderiza errores, color rojo
printError :: Error -> String
printError e = render (red $ text (show e))

-- Renderiza errores de Happy, color rojo
printHappyError :: String -> String
printHappyError s = render (red $ text s)

-- Renderiza el prompt, color verde
printPrompt :: Env -> String
printPrompt e = render (green $ text ("~/" ++ show e ++ "$ "))

-- Renderiza las tareas
-- Rojo, tarea no completada 
-- Verde, tarea completada
printTask :: Task -> String
printTask t@(Task _ _ True _ _) = render (green $ text (show t))
printTask t@(Task _ _ False _ _) = render (red $ text (show t))

-- Renderiza las carpetas, color azul
printFolder :: Folder -> String
printFolder f = render (blue $ text (show f))

-- Formatea una lista de tareas para mostrarlas por pantalla
showTasks :: [Task] -> String
showTasks [] = ""
showTasks (t:[]) = printTask t
showTasks (t:ts) = printTask t ++ "\n" ++ showTasks ts

-- Formatea las carpetas recibidas para mostrarlas en pantalla
showFolders :: [Folder] -> String
showFolders [] = ""
showFolders (f:fs) = printFolder f ++ "    " ++ showFolders fs

-- Devuelve el contenido de la carpeta recibida en forma de string, separado por carpetas y tareas
showEnv :: Env -> String
showEnv ((Folder _ fs ts), _, _) = 
    case (M.null fs, M.null ts) of
     (True, True) -> ""
     (True, False) -> showTasks $ sortMap ts
     (False, True) -> showFolders $ sortMap fs
     (False, False) -> showFolders (sortMap fs) ++ showTasks (sortMap ts)
    where sortMap m = map snd $ sort $ M.toList m

-- Funcion de renderizado
render :: Doc -> String
render = flip displayS "" . renderPretty 0.5 80
