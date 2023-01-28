module Monad.Env
  ( Env
  , getActualFolder
  , getRootFolder
  , getRoute
  , getProfileName
  ) where

import Structures.Folder (Folder(..))
import Structures.Route (Route(..))

-- (actual folder, root folder, actual route, profile name)
type Env = (Folder, Folder, Route, String)

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
getProfileName :: Env -> String
getProfileName (_, _, _, pn) = pn
