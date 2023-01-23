module Monad.Env 
    (Env,
     getActualFolder, getRootFolder, getRoute, getProfileName)
    where
    
import Profile.Profile (ProfileName)
import Structures.Folder (Folder (..))
import Structures.Route (Route (..))

-- (actual folder, root folder, actual route)
type Env = (Folder, Folder, Route, ProfileName)

-- Devuelve la carpeta actual
getActualFolder :: Env -> Folder
getActualFolder (f, _, _, _) = f

-- Devuelve la carpeta root
getRootFolder :: Env -> Folder
getRootFolder (_, f, _, _) = f

-- Devuelve la ruta
getRoute :: Env -> Route
getRoute (_, _, r, _) = r

-- Devuelve el nombre del perfil
getProfileName :: Env -> ProfileName
getProfileName (_, _, _, pn) = pn