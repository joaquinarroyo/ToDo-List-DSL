module Extra.Pp
    (   printError, 
        printPrompt,
        printHappyError,
        showEnv
    ) 
    where

import Structures.Task
import Structures.Folder
import Structures.Env
import Extra.Error
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.List as L (sort)

printError :: Error -> String
printError e = render (red $ text (show e))

printHappyError :: String -> String
printHappyError s = render (red $ text s)

printPrompt :: String -> String
printPrompt s = render (green $ text s)

printTask :: Task -> String
printTask t@(Task _ _ True _ _) = render (green $ text (show t))
printTask t@(Task _ _ False _ _) = render (red $ text (show t))

printFolder :: Folder -> String
printFolder f = render (blue $ text (show f))

render :: Doc -> String
render = flip displayS "" . renderPretty 0.5 80

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
showEnv ((Folder _ [] []), _ ,_) = ""
showEnv ((Folder _ fs []), _ ,_) = showFolders (L.sort fs)
showEnv ((Folder _ [] ts), _ ,_) = showTasks (L.sort ts)
showEnv ((Folder _ fs ts), _, _) = showFolders (L.sort fs) ++ showTasks (L.sort ts)

