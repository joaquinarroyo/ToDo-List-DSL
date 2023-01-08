module Structures.Folder 
    (Folder (..),
     newdir, addTask, addTaskToRoot, addDir, addDirToRoot,
     deleteTask, deleteTaskFromRoot, deleteDir, deleteDirFromRoot,
     editTask', editTaskFromRoot, editTaskB', editTaskBFromRoot,
     editTaskP', editTaskPFromRoot, editTaskT', editTaskTFromRoot,
     editDir, editDirFromRoot, taskInFolder, folderInFolder,
     folderInParent, findFolder, findParentFolder, search, searchRecursive)
    where

import Data.Map
import Eval.EvalFilter
import Filter.AST (Filter (..))
import Prelude hiding (lookup)
import Structures.Route (Route (..), backRoute)
import Structures.Task as T hiding (deleteTask)

-- Carpeta con nombre, subcarpetas, tareas y ruta a carpeta padre
data Folder = Folder { fname :: String, 
                       folders :: Map Name Folder, 
                       tasks :: Map Name Task } 
                       deriving (Eq)

instance Show Folder where
    show (Folder n fs ts) = n

instance Ord Folder where
    compare (Folder n _ _) (Folder n' _ _) = compare n n'

-- Crea una nueva carpeta con el nombre recibido y setea la carpeta padre
newdir :: Name -> Folder
newdir n = Folder { fname = n, folders = empty, tasks = empty }

-- Crea una nueva tarea con el nombre recibido sobre la carpeta recibida
addTask :: Task -> Folder -> Folder
addTask t f = f { tasks = insert (tname t) t (tasks f) }

-- Agrega una carpeta a la carpeta recibida
addDir :: Folder -> Folder -> Folder
addDir f' f = f { folders = insert (fname f') f' (folders f) }

-- Agrega una tarea en la ruta recibida
-- Es necesario para mantener actualizado el directorio root
addTaskToRoot :: Task -> Folder -> Route -> Folder
addTaskToRoot t f r = let f' = addToRoot t f r
                      in f' { tasks = insert (tname t) t (tasks f')}

-- Agrega una carpeta en la ruta recibida
-- Es necesario para mantener actualizado el directorio root
addDirToRoot :: Folder -> Folder -> Route -> Folder
addDirToRoot f f' r = let f'' = addToRoot f f' r
                      in f'' { folders = insert (fname f) f (folders f'')}

addToRoot :: a -> Folder -> Route -> Folder
addToRoot f f' Empty = f'
addToRoot f (Folder n fs ts) (Route n' r) = 
    case lookup n' fs of
        Just f' -> Folder n (insert n' (addToRoot f f' r) fs) ts
        Nothing -> Folder n fs ts

-- Busca una tarea con nombre 'n' en la lista de tareas recibida
deleteTask :: Name -> Folder -> Folder
deleteTask n f = f { tasks = delete n (tasks f) }

-- Elimina una carpeta de la carpeta recibida
deleteDir :: Name -> Folder -> Folder
deleteDir n f = f { folders = delete n (folders f) }

-- Elimina una tarea con nombre 'n' en la ruta recibida
-- Es necesario para mantener actualizado el directorio root
deleteTaskFromRoot :: Name -> Folder -> Route -> Folder
deleteTaskFromRoot n f r = let f' = deleteFromRoot n f r
                           in f' { tasks = delete n (tasks f') }

-- Elimina una carpeta en la ruta recibida
-- Es necesario para mantener actualizado el directorio root
deleteDirFromRoot :: Name -> Folder -> Route -> Folder
deleteDirFromRoot n f r = let f' = deleteFromRoot n f r
                          in f' { folders = delete n (folders f') }

-- Funcion auxiliar para deletes de root
deleteFromRoot :: Name -> Folder -> Route -> Folder
deleteFromRoot n f Empty = f
deleteFromRoot n (Folder n' fs ts) (Route n'' r) = 
    case lookup n'' fs of
        Just f' -> Folder n' (insert n'' (deleteFromRoot n f' r) fs) ts
        Nothing -> Folder n' fs ts


-- Edita una tarea con nombre 'n' en la lista de tareas recibida
editTask' :: Name -> Field -> String -> Folder -> Folder
editTask' n Name s fl@(Folder n' fs ts) = case lookup n ts of
                                           Just t' -> fl { tasks = insert s (T.editTask t' Name s) ts' }
                                           Nothing -> fl
        where ts' = delete n ts
editTask' n f s fl@(Folder n' fs ts) = case lookup n ts of
                                        Just t' -> fl { tasks = insert n (T.editTask t' f s) ts }
                                        Nothing -> fl

-- Analogo a editTask pero para timestamp
editTaskT' :: Name -> Date -> Folder -> Folder
editTaskT' n t fl@(Folder n' fs ts) = case lookup n ts of
                                        Just t' -> fl { tasks = insert n (T.editTaskT t' t) ts }
                                        Nothing -> fl


-- Analogo a editTask pero para completed
editTaskB' :: Name -> Bool -> Folder -> Folder
editTaskB' n b fl@(Folder n' fs ts) = case lookup n ts of
                                        Just t' -> fl { tasks = insert n (T.editTaskB t' b) ts }
                                        Nothing -> fl

-- Analogo a editTask pero para priority
editTaskP' :: Name -> Priority -> Folder -> Folder
editTaskP' n p fl@(Folder n' fs ts) = case lookup n ts of
                                        Just t' -> fl { tasks = insert n (T.editTaskP t' p) ts }
                                        Nothing -> fl

-- Edita una tarea con nombre 'n' en la ruta recibida
-- Es necesario para mantener actualizado el directorio root
editTaskFromRoot :: Name -> Field -> String -> Folder -> Route -> Folder
editTaskFromRoot n f s f' r = let f'' = editFromRoot n f' r
                                  ts = tasks f''
                                  ts' = delete n ts
                              in case f of
                                Name -> f'' { tasks = insert n (T.editTask (ts' ! n) f s) ts'}
                                Description -> f'' { tasks = insert n (T.editTask (ts ! n) f s) ts}

-- Analogo a editTaskFromRoot para Completed
editTaskBFromRoot :: Name -> Completed -> Folder -> Route -> Folder
editTaskBFromRoot n b f r = let f' = editFromRoot n f r
                                ts = tasks f'
                            in f' { tasks = insert n (T.editTaskB (ts ! n) b) ts}

-- Analogo a editTaskFromRoot para Priority
editTaskPFromRoot :: Name -> Priority -> Folder -> Route -> Folder
editTaskPFromRoot n p f r = let f' = editFromRoot n f r
                                ts = tasks f'
                            in f' { tasks = insert n (T.editTaskP (ts ! n) p) ts}

-- Analogo a editTaskFromRoot para Date
editTaskTFromRoot :: Name -> Date -> Folder -> Route -> Folder
editTaskTFromRoot n d f r = let f' = editFromRoot n f r
                                ts = tasks f'
                            in f' { tasks = insert n (T.editTaskT (ts ! n) d) ts}

-- Funcion auxiliar para edits de root
editFromRoot :: Name -> Folder -> Route -> Folder
editFromRoot n f Empty = f
editFromRoot n (Folder n' fs ts) (Route n'' r) = 
    case lookup n'' fs of
        Just f' -> Folder n' (insert n'' (editFromRoot n f' r) fs) ts
        Nothing -> Folder n' fs ts

-- Edita el nombre de la carpeta recibida
editDir :: Name -> Folder -> Folder
editDir n f = f { fname = n }

-- Edita el nombre de la carpeta en la ruta recibida
-- Es necesario para mantener actualizado el directorio root
editDirFromRoot :: Name -> Folder -> Route -> Folder
editDirFromRoot n (Folder n' fs ts) (Route n'' Empty) =
    case lookup n'' fs of
        Just f' -> Folder n' (insert n (editDir n f') fs') ts
        Nothing -> Folder n' fs ts
    where fs' = delete n'' fs
editDirFromRoot n (Folder n' fs ts) (Route n'' r) = 
    case lookup n'' fs of
        Just f' -> Folder n' (insert n'' (editDirFromRoot n f' r) fs) ts
        Nothing -> Folder n' fs ts

-- Chequea si existe una tarea con nombre 'n' en la carpeta recibida
taskInFolder :: Name -> Folder -> Bool
taskInFolder n (Folder _ _ ts) = lookupInFolder n ts

-- Chequea si existe una carpeta con nombre 'n' en la carpeta recibida
folderInFolder :: Name -> Folder -> Bool
folderInFolder n (Folder _ fs _) = lookupInFolder n fs

-- Funcion auxiliar para buscar en mapa
lookupInFolder :: Name -> Map Name a -> Bool
lookupInFolder n m = case lookup n m of
                        Just _ -> True
                        _ -> False

-- Chequea si existe una carpeta con nombre 'n' en la carpeta recibida o en alguna de sus subcarpetas
folderInParent :: Name -> Route -> Folder -> Bool
folderInParent n Empty f = folderInFolder n f
folderInParent n (Route n' r) f = case lookup n' (folders f) of
                                    Just f' -> folderInParent n r f'
                                    Nothing -> False

-- Busca una tarea con nombre 'n' en la carpeta recibida
findFolder :: Route -> Folder -> Maybe Folder
findFolder Empty f = Just f
findFolder (Route n r) f = case lookup n (folders f) of
                            Just f' -> findFolder r f'
                            Nothing -> Nothing

-- Busca una tarea con nombre 'n' a partir de la ruta recibida, desde el directorio root
findParentFolder :: Folder -> Route -> Maybe Folder
findParentFolder f r = findFolder route f
    where route = backRoute r

-- Busca tareas que cumplan con el filtro recibido en la carpeta recibida
search :: (MonadFail m) => Filter -> Folder -> m [Task]
search f (Folder _ _ ts) = search' f (toList ts)

search' :: (MonadFail m) => Filter -> [(Name, Task)] -> m [Task]
search' f [] = return []
search' f ((n,t):ts) = do b <- evalFilter t f
                          ts' <- search' f ts
                          if b then return (t:ts') else return ts'

-- Busca tareas que cumplan con el filtro recibido en la carpeta recibida y recursivamente
searchRecursive :: (MonadFail m) => Filter -> Folder -> m [Task]
searchRecursive f (Folder _ fs ts) = do ts' <- search' f (toList ts)
                                        ts'' <- searchRecursive' f (toList fs)
                                        return (ts' ++ ts'')

-- Busca tareas que cumplan con el filtro recibido recursivamente desde las carpetas recibidas
searchRecursive' :: (MonadFail m) => Filter -> [(Name, Folder)] -> m [Task]
searchRecursive' f [] = return []
searchRecursive' f ((_,f'):fs) = do ts' <- searchRecursive f f'
                                    ts'' <- searchRecursive' f fs
                                    return (ts' ++ ts'')

