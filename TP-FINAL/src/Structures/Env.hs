module Structures.Env where
import Structures.Folder
import Structures.Task
import Structures.Route
import qualified Data.List as L (sort)

type Env = (Folder, Folder, Route)

getActualFolder :: Env -> Folder
getActualFolder (f, _, _) = f

getRootFolder :: Env -> Folder
getRootFolder (_, f, _) = f

getRoute :: Env -> Route
getRoute (_, _, r) = r

showRoute :: Env -> String
showRoute e = show (getRoute e)

-- ls command
-- Devuelve el contenido de la carpeta recibida en forma de string, separado por carpetas y tareas
ls :: Env -> String
ls ((Folder _ [] []), _ ,_) = "No folders\nNo tasks"
ls ((Folder _ fs []), _ ,_) = "Folders\n" ++ showFolders (L.sort fs) ++ "\nNo tasks"
ls ((Folder _ [] ts), _ ,_) = "No folders \nTasks\n" ++ showTasks (L.sort ts)
ls ((Folder _ fs ts), _, _) = "Folders\n" ++ showFolders (L.sort fs) ++ "\nTasks\n" ++ showTasks (L.sort ts)