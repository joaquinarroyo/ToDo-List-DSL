module Monads.Monad 
    (MonadState (..), MonadError (..), State (..))
    where

import Control.Monad (liftM, ap)
import Extra.Error
import Filter.AST
import Structures.Env
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
                            Right (x, (f1, f2, r)) -> runState (g x) (f1, f2, r))
                            
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
    -- Edita una tarea (Campos Name y Description)
    editTask :: Name -> Field -> String -> m ()
    -- Edita una tarea (Campo Priority)
    editTaskP :: Name -> Integer -> m ()
    -- Edita una tarea (Campo Timestamp)
    editTaskT :: Name -> Date -> m ()
    -- Edita una tarea (Campo Completed)
    editTaskB :: Name -> Bool -> m ()
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
    setFolder f = State (\(_, p, r) -> Right ((), (f, p, r)))
    getRoute = State (\e@(_, _, r) -> Right (r, e))
    setRoute route = State (\(f, p, r) -> Right ((), (f, p, R.addRoute r route)))
    backRoute = State (\(f, p, r) -> Right ((), (f, p, R.backRoute r)))
    addTask name desc prio time = State (\(f, p, r) -> 
        if R.inRoot r
        then Left CannotCreateTaskInRootDir
        else Right ((), (F.addTask task f, F.addTaskToRoot task p r, r)))
        where task = newTask name desc prio time
    addDir name = State (\(f, p, r) -> Right ((), (F.addDir newfolder f, F.addDirToRoot newfolder p r, r)))
        where newfolder = newdir name
    deleteTask name = State (\(f, p, r) -> Right ((), (F.deleteTask name f, F.deleteTaskFromRoot name p r,r)))
    deleteDir name = State (\(f, p, r) -> Right ((), (F.deleteDir name f, F.deleteDirFromRoot name p r, r)))
    editTask name field value = 
        State (\(f, p, r) -> Right ((), (F.editTask' name field value f, F.editTaskFromRoot name field value p r, r)))
    editTaskP name prio = 
        State (\(f, p, r) -> Right ((), (F.editTaskP' name prio f, F.editTaskPFromRoot name prio p r, r)))
    editTaskB name comp =
        State (\(f, p, r) -> Right ((), (F.editTaskB' name comp f,F.editTaskBFromRoot name comp p r, r)))
    editTaskT name time =
        State (\(f, p, r) -> Right ((), (F.editTaskT' name time f, F.editTaskTFromRoot name time p r, r)))
    editDir name = State (\(f, p, r) -> 
                    if R.inRoot r
                    then Left CannotEditRootDir
                    else Right ((), (F.editDir name f, F.editDirFromRoot name p r, R.editRoute r name)))
    taskInFolder name = State (\e@(f, p, r) -> Right (F.taskInFolder name f, e))
    folderInFolder name = State (\e@(f, p, r) -> Right (F.folderInFolder name f, e))
    folderInParent name = State (\e@(f, p, r) -> Right (F.folderInParent name r f, e))
    findFolder route = State (\e@(f, p, r) -> Right (F.findFolder route f, e))
    findBackFolder = State (\e@(f, p, r) -> Right (case F.findParentFolder p r of
                                                Nothing -> Nothing
                                                Just f' -> Just f', e))
    search filter = State (\e@(f, p, r) -> do t <- F.search filter f
                                              return (t, e))
    searchR filter = State (\e@(f, p, r) -> do t <- F.searchRecursive filter f
                                               return (t, e))

-- MonadError
class (Monad m, MonadFail m) => MonadError m where
    throw :: Error -> m a

instance MonadError State where
    throw e = State (\f -> Left e)

-- MonadFail es utilizada ya que el parser de LocalTime la utiliza
instance MonadFail State where
    fail _ = State (\f -> Left WrongDateFormat)

instance MonadFail (Either Error) where
    fail _ = Left WrongDateFormat

