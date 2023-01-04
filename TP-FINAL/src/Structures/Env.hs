module Structures.Env where
import Structures.Folder
import Structures.Task
import Structures.Route

type Env = (Folder, Folder, Route)

getActualFolder :: Env -> Folder
getActualFolder (f, _, _) = f

getRootFolder :: Env -> Folder
getRootFolder (_, f, _) = f

getRoute :: Env -> Route
getRoute (_, _, r) = r

showRoute :: Env -> String
showRoute e = show (getRoute e)