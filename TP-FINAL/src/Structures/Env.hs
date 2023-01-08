module Structures.Env 
    (Env,
     getActualFolder, getRootFolder, getRoute)
    where
    
import Structures.Folder (Folder (..))
import Structures.Route (Route (..))

-- (actual folder, root folder, actual route)
type Env = (Folder, Folder, Route)

-- Devuelve la carpeta actual
getActualFolder :: Env -> Folder
getActualFolder (f, _, _) = f

-- Devuelve la carpeta root
getRootFolder :: Env -> Folder
getRootFolder (_, f, _) = f

-- Devuelve la ruta
getRoute :: Env -> Route
getRoute (_, _, r) = r