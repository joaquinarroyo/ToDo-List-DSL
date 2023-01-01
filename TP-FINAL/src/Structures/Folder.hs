module Structures.Folder where
import Structures.Task as T
import Structures.Route
import Eval.EvalFilter
import Filter.AST
import Extra.Lib (localTime)

-- Carpeta con nombre, subcarpetas, tareas y ruta a carpeta padre
data Folder = Folder Name [Folder] [Task] deriving (Eq)

instance Show Folder where
    show (Folder n fs ts) = n

instance Ord Folder where
    compare (Folder n _ _) (Folder n' _ _) = compare n n'

-- Devuelve el nombre de la carpeta recibida
getFolderName :: Folder -> Name
getFolderName (Folder n _ _) = n

-- Devuelve las subcarpetas de la carpeta recibida
getFolderFolders :: Folder -> [Folder]
getFolderFolders (Folder _ fs _ ) = fs

-- Devuelve las tareas de la carpeta recibida
getFolderTasks :: Folder -> [Task]
getFolderTasks (Folder _ _ ts) = ts

-- Crea una nueva carpeta con el nombre recibido y setea la carpeta padre
newdir :: Name -> Folder
newdir n = Folder n [] []

-- Crea una nueva tarea con el nombre recibido sobre la carpeta recibida
addTask :: Task -> Folder -> Folder
addTask t (Folder n fs ts) = Folder n fs (t : ts)

-- Agrega una tarea a la carpeta recibida en la ruta recibida
-- Este metodo es necesario para mantener actualizado el directorio 'root'
addTaskToRoot :: Task -> Folder -> Route -> Folder
addTaskToRoot t (Folder n fs ts) Empty = Folder n fs (t : ts)
addTaskToRoot t (Folder n fs ts) (Route n' r) = Folder n (map (\f -> if n' == getFolderName f
                                                                     then addTaskToRoot t f r
                                                                     else f) fs) ts

-- Agrega una carpeta a la carpeta recibida
addDir :: Folder -> Folder -> Folder
addDir f (Folder n fs ts) = Folder n (f : fs) ts

-- Agrega una carpeta a la carpeta recibida en la ruta recibida
-- Este metodo es necesario para mantener actualizado el directorio 'root'
addDirToRoot :: Folder -> Folder -> Route -> Folder
addDirToRoot f (Folder n fs ts) Empty = Folder n (f : fs) ts
addDirToRoot f (Folder n fs ts) (Route n' r) = Folder n (map (\f' -> if n' == getFolderName f'
                                                                     then addDirToRoot f f' r
                                                                     else f') fs) ts

-- Busca una tarea con nombre 'n' en la lista de tareas recibida
deleteTask :: Name -> Folder -> Folder
deleteTask n (Folder n' fs ts) = Folder n' fs (filter (\t -> n /= getTaskName t) ts)

-- Elimina una tarea de la carpeta recibida en la ruta recibida
-- Este metodo es necesario para mantener actualizado el directorio 'root'
deleteTaskFromRoot :: Name -> Folder -> Route -> Folder
deleteTaskFromRoot n (Folder n' fs ts) Empty = Folder n' fs (filter (\t -> n /= getTaskName t) ts)
deleteTaskFromRoot n (Folder n' fs ts) (Route n'' r) = Folder n' (map (\f -> if n'' == getFolderName f
                                                                           then deleteTaskFromRoot n f r
                                                                           else f) fs) ts

-- Elimina una carpeta de la carpeta recibida
deleteDir :: Name -> Folder -> Folder
deleteDir n (Folder n' fs ts) = Folder n' (filter (\f -> n /= getFolderName f) fs) ts

-- Elimina una carpeta de la carpeta recibida en la ruta recibida
-- Este metodo es necesario para mantener actualizado el directorio 'root'
deleteDirFromRoot :: Name -> Folder -> Route -> Folder
deleteDirFromRoot n (Folder n' fs ts) Empty = Folder n' (filter (\f -> n /= getFolderName f) fs) ts
deleteDirFromRoot n (Folder n' fs ts) (Route n'' r) = Folder n' (map (\f -> if n'' == getFolderName f
                                                                          then deleteDirFromRoot n f r
                                                                          else f) fs) ts

-- Edita una tarea con nombre 'n' en la lista de tareas recibida
editTask :: Name -> Field -> String -> Folder -> Folder
editTask n f s (Folder n' fs ts) = Folder n' fs (map (\t -> if n == getTaskName t
                                                           then T.editTask t f s
                                                           else t) ts)

-- Edita una tarea de la carpeta recibida en la ruta recibida
-- Este metodo es necesario para mantener actualizado el directorio 'root'
editTaskFromRoot :: Name -> Field -> String -> Folder -> Route -> Folder
editTaskFromRoot n f s (Folder n' fs ts) Empty = Folder n' fs (map (\t -> if n == getTaskName t
                                                                         then T.editTask t f s
                                                                         else t) ts)
editTaskFromRoot n f s (Folder n' fs ts) (Route n'' r) = Folder n' (map (\f' -> if n'' == getFolderName f'
                                                                             then editTaskFromRoot n f s f' r
                                                                             else f') fs) ts

-- Analogo a editTask pero para completed
editTaskB :: Name -> Bool -> Folder -> Folder
editTaskB n b (Folder n' fs ts) = Folder n' fs (map (\t -> if n == getTaskName t
                                                          then T.editTaskB t b
                                                          else t) ts)

editTaskFromRootB :: Name -> Bool -> Folder -> Route -> Folder
editTaskFromRootB n b (Folder n' fs ts) Empty = Folder n' fs (map (\t -> if n == getTaskName t
                                                                        then T.editTaskB t b
                                                                        else t) ts)
editTaskFromRootB n b (Folder n' fs ts) (Route n'' r) = Folder n' (map (\f' -> if n'' == getFolderName f'
                                                                             then editTaskFromRootB n b f' r
                                                                             else f') fs) ts      

-- Analogo a editTask pero para priority
editTaskP :: Name -> Priority -> Folder -> Folder
editTaskP n p (Folder n' fs ts) = Folder n' fs (map (\t -> if n == getTaskName t
                                                          then T.editTaskP t p
                                                          else t) ts)

editTaskFromRootP :: Name -> Priority -> Folder -> Route -> Folder
editTaskFromRootP n p (Folder n' fs ts) Empty = Folder n' fs (map (\t -> if n == getTaskName t
                                                                        then T.editTaskP t p
                                                                        else t) ts)
editTaskFromRootP n p (Folder n' fs ts) (Route n'' r) = Folder n' (map (\f' -> if n'' == getFolderName f'
                                                                             then editTaskFromRootP n p f' r
                                                                             else f') fs) ts

-- Analogo a editTask pero para timestamp
editTaskT :: Name -> Date -> Folder -> Folder
editTaskT n t (Folder n' fs ts) = Folder n' fs (map (\t' -> if n == getTaskName t'
                                                          then T.editTaskT t' t
                                                          else t') ts)

editTaskFromRootT :: Name -> Date -> Folder -> Route -> Folder
editTaskFromRootT n t (Folder n' fs ts) Empty = Folder n' fs (map (\t' -> if n == getTaskName t'
                                                                        then T.editTaskT t' t
                                                                        else t') ts)
editTaskFromRootT n t (Folder n' fs ts) (Route n'' r) = Folder n' (map (\f' -> if n'' == getFolderName f'
                                                                             then editTaskFromRootT n t f' r
                                                                             else f') fs) ts

-- Edita el nombre de la carpeta recibida
editDir :: Name -> Folder -> Folder
editDir n (Folder _ fs ts) = Folder n fs ts

-- Edita el nombre de la carpeta recibida en la ruta recibida
-- Este metodo es necesario para mantener actualizado el directorio 'root'
editDirFromRoot :: Name -> Folder -> Route -> Folder
editDirFromRoot n (Folder n' fs ts) Empty = Folder n fs ts
editDirFromRoot n (Folder n' fs ts) (Route n'' r) = Folder n' (map (\f -> if n'' == getFolderName f
                                                                          then editDirFromRoot n f r
                                                                          else f) fs) ts

-- Busca una carpeta a partir de una ruta en el directorio recibido
findFolder :: Route -> Folder -> Maybe Folder
findFolder Empty f = Just f
findFolder (Route n r) (Folder _ fs _) = case filter (\f -> n == getFolderName f) fs of
                                            (x:xs) -> findFolder r x
                                            [] -> Nothing

-- Busca una carpeimport Data.Fixedtwhere isFlag c = c `elem` "-r"a padre a partir de una ruta y el directorio 'root'
findBackFolder :: Route -> Folder -> Maybe Folder
findBackFolder r f = findFolder route f
    where route = backRoute r

-- Chequea si existe una tarea con nombre 'n' en la carpeta recibida
taskInFolder :: Name -> Folder -> Bool
taskInFolder n (Folder _ _ ts) = case findTask n ts of
                                  Just _ -> True
                                  Nothing -> False

-- Chequea si existe una carpeta con nombre 'n' en la carpeta recibida
folderInFolder :: Name -> Folder -> Bool
folderInFolder n (Folder _ fs _) = case filter (\f -> n == getFolderName f) fs of
                                    (x:xs) -> True
                                    [] -> False

-- Chequea si existe una carpeta con nombre 'n' en la ruta recibida
-- Este metodo es necesario para chequear el cambio de nombre de carpetas
folderInFolderRoot :: Name -> Folder -> Route -> Bool
folderInFolderRoot n (Folder _ fs _) Empty = case filter (\f -> n == getFolderName f) fs of
                                              (x:xs) -> True
                                              [] -> False
folderInFolderRoot n (Folder _ fs _) (Route n' r) = case filter (\f -> n' == getFolderName f) fs of
                                                     (x:xs) -> folderInFolderRoot n x r
                                                     [] -> False

-- Busca tareas que cumplan con el filtro recibido en la carpeta recibida
search :: (Monad m, MonadFail m) => Filter -> Folder -> m [Task]
search f (Folder _ _ ts) = search' f ts

search' :: (Monad m, MonadFail m) => Filter -> [Task] -> m [Task]
search' f [] = return []
search' f (t:ts) = do b <- evalFilter t f
                      if b
                      then do ts' <- search' f ts
                              return (t:ts')
                      else search' f ts

-- Busca tareas que cumplan con el filtro recibido en la carpeta recibida y recursivamente
searchRecursive :: (Monad m, MonadFail m) => Filter -> Folder -> m [Task]
searchRecursive f (Folder _ fs ts) = do ts' <- search' f ts
                                        ts'' <- searchRecursive' f fs
                                        return (ts' ++ ts'')

searchRecursive' :: (Monad m, MonadFail m) => Filter -> [Folder] -> m [Task]
searchRecursive' f [] = return []
searchRecursive' f (f':fs) = do ts' <- searchRecursive f f'
                                ts'' <- searchRecursive' f fs
                                return (ts' ++ ts'')

-- Formatea las carpetas recibidas para mostrarlas en pantalla
showFolders :: [Folder] -> String
showFolders [] = ""
showFolders (f:fs) = show f ++ "    " ++ showFolders fs
