module Monad.State
    (MonadState (..), MonadError (..), State (..))
    where

import Control.Monad (liftM, ap)
import Extra.Error
import Filter.AST
import Filter.Lib as L (search, searchRecursive, checkFilter)
import Monad.Env
import Structures.Folder as F
import Structures.Route as R
import Structures.Task

-- Estado
newtype State a = State { runState :: Env -> Either Error (a, Env) }

instance Functor State where
    fmap = liftM

instance Applicative State where
    pure = return
    (<*>) = ap

instance Monad State where
    return x = State (\f -> Right (x, f))
    m >>= g = State (\f -> case runState m f of
                            Left e -> Left e
                            Right (x, (f1, f2, r, pn)) -> runState (g x) (f1, f2, r, pn))
                            
-- MonadState
class Monad m => MonadState m where
    -- Setea la folder actual
    setFolder :: Folder -> m ()
    -- Devuelve la ruta actual
    getRoute :: m Route
    -- Setea la ruta
    setRoute :: Route -> m ()
    -- Devuelve la ruta sin el ultimo directorio
    backRoute :: m ()
    -- Agrega una tarea al folder system
    addTask :: Name -> Description -> Priority -> Date -> m ()
    -- Agrega una carpeta al folder system
    addDir :: Name -> m ()
    -- Borra una tarea
    deleteTask :: Name -> m ()
    -- Borra un directorio
    deleteDir :: Name -> m ()
    -- Edita una tarea
    editTask :: (Read a, Show a) => Name -> Field -> a -> m ()
    -- Edita el nombre de un directorio
    editDir :: Name -> m ()
    -- Chequea si la tarea esta en la carpeta actual
    taskInFolder :: Name -> m Bool
    -- Chequea si la carpeta esta en la carpeta actual
    folderInFolder :: Name -> m Bool
    -- Chequea si hay una carpeta con el nombre recibido a mismo nivel que la carpeta actual
    folderInParent :: Name -> m Bool
    -- Busca una carpeta a partir de una ruta
    findFolder :: Route -> m (Maybe Folder)
    -- Busca la carpeta padre
    findBackFolder :: m (Maybe Folder)
    -- Busca las tareas que cumplan con el filtro en la carpeta actual
    search :: Filter -> m [Task]
    -- Busca las tareas que cumplan con el filtro recursivamente desde la carpeta actual
    searchR :: Filter -> m [Task]

instance MonadState State where
    setFolder f = State (\(_, p, r, pn) -> Right ((), (f, p, r, pn)))
    getRoute = State (\e@(_, _, r, _) -> Right (r, e))
    setRoute route = State (\(f, p, r, pn) -> Right ((), (f, p, R.addRoute r route, pn)))
    backRoute = State (\(f, p, r, pn) -> Right ((), (f, p, R.backRoute r, pn)))
    addTask name desc prio time = State (\(f, p, r, pn) -> 
        if R.inRoot r
        then Left CannotCreateTaskInRootDir
        else Right ((), (F.addTask task f, F.addTaskToRoot task p r, r, pn)))
        where task = newTask name desc prio time
    addDir name = State (\(f, p, r, pn) -> 
        Right ((), (F.addDir newfolder f, F.addDirToRoot newfolder p r, r, pn)))
        where newfolder = newdir name
    deleteTask name = State (\(f, p, r, pn) -> 
        Right ((), (F.deleteTask name f, F.deleteTaskFromRoot name p r, r, pn)))
    deleteDir name = State (\(f, p, r, pn) -> 
        Right ((), (F.deleteDir name f, F.deleteDirFromRoot name p r, r, pn)))
    editTask name field value = 
        State (\(f, p, r, pn) -> 
            Right ((), (F.editTask name field value f, F.editTaskFromRoot name field value p r, r, pn)))
    editDir name = State (\(f, p, r, pn) -> 
                    if R.inRoot r
                    then Left CannotEditRootDir
                    else Right ((), (f { fname = name} , F.editDirFromRoot name p r, R.editRoute r name, pn)))
    taskInFolder name = State (\e@(f, p, r, pn) -> Right (F.taskInFolder name f, e))
    folderInFolder name = State (\e@(f, p, r, pn) -> Right (F.folderInFolder name f, e))
    folderInParent name = State (\e@(f, p, r, pn) -> Right (F.folderInParent name r f, e))
    findFolder route = State (\e@(f, p, r, pn) -> Right (F.findFolder route f, e))
    findBackFolder = State (\e@(f, p, r, pn) -> Right (
                                            case F.findParentFolder p r of
                                                Nothing -> Nothing
                                                Just f' -> Just f', e))
    search filter = State (\e@(f, p, r, pn) -> case L.checkFilter filter of
                                                Left e -> Left e
                                                Right _ -> Right (L.search filter f, e))
    searchR filter = State (\e@(f, p, r, pn) -> case L.checkFilter filter of
                                                 Left e -> Left e
                                                 Right _ -> Right (L.searchRecursive filter f, e))

-- MonadError
class (Monad m) => MonadError m where
    throw :: Error -> m a

instance MonadError State where
    throw e = State (\f -> Left e)