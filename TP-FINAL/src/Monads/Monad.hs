module Monads.Monad where
import Structures.Task
import Structures.Folder as F
import Structures.Route as R
import Structures.Env
import Filter.AST
import Extra.Error
import Control.Monad (liftM, ap)

-- (a, folderActual, folderAnterior)
newtype State a = State { runState :: Env -> Either Error (a, Env) }

instance Monad State where
    return x = State (\f -> Right (x, f))
    m >>= g = State (\f -> case runState m f of
                            Left e -> Left e
                            Right (x, (f1, f2, r)) -> runState (g x) (f1, f2, r))

-- Para calmar al GHC
instance Functor State where
    fmap = liftM

instance Applicative State where
    pure  = return
    (<*>) = ap

class Monad m => MonadState m where
    getFolder :: m Folder
    setFolder :: Folder -> m ()
    getRoute :: m Route
    setRoute :: Route -> m ()
    backRoute :: m ()
    addTask :: Name -> Description -> Priority -> Date -> m ()
    addDir :: Name -> m ()
    deleteTask :: Name -> m ()
    deleteDir :: Name -> m ()
    editTask :: Name -> Field -> String -> m ()
    editTaskP :: Name -> Integer -> m ()
    editTaskT :: Name -> Date -> m ()
    editTaskB :: Name -> Bool -> m ()
    editDir :: Name -> m ()
    taskInFolder :: Name -> m Bool
    folderInFolder :: Name -> m Bool
    folderInBackFolder :: Name -> m Bool
    findFolder :: Route -> m (Maybe Folder)
    findBackFolder :: m (Maybe Folder)
    search :: Filter -> m [Task]
    searchR :: Filter -> m [Task]

class (Monad m, MonadFail m) => MonadError m where
    throw :: Error -> m a

instance MonadState State where
    getFolder = State (\e@(f, _, _) -> Right (f, e))
    setFolder f = State (\(_, p, r) -> Right ((), (f, p, r)))
    getRoute = State (\e@(f, p, r) -> Right (r, e))
    setRoute route = State (\e@(f, p, r) -> Right ((), (f, p, R.addRoute r route)))
    backRoute = State (\e@(f, p, r) -> Right ((), (f, p, R.backRoute r)))
    addTask name desc prio time = State (\(f, p, r) -> 
        if R.inRoot r
        then Left CannotCreateTaskInRootDir
        else Right ((), (F.addTask task f, F.addTaskToRoot task p r, r)))
        where task = newTask name desc prio time
    addDir name = State (\(f, p, r) -> Right ((), (F.addDir newFolder f, F.addDirToRoot newFolder p r, r)))
        where newFolder = newdir name
    deleteTask name = State (\(f, p, r) -> Right ((), (F.deleteTask name f, F.deleteTaskFromRoot name p r, r)))
    deleteDir name = State (\(f, p, r) -> Right ((), (F.deleteDir name f, F.deleteDirFromRoot name p r, r)))
    editTask name field value = 
        State (\(f, p, r) -> Right ((), (F.editTask name field value f, F.editTaskFromRoot name field value p r, r)))
    editTaskP name prio = 
        State (\(f, p, r) -> Right ((), (F.editTaskP name prio f, F.editTaskFromRootP name prio p r, r)))
    editTaskB name comp =
        State (\(f, p, r) -> Right ((), (F.editTaskB name comp f, F.editTaskFromRootB name comp p r, r)))
    editTaskT name time =
        State (\(f, p, r) -> Right ((), (F.editTaskT name time f, F.editTaskFromRootT name time p r, r)))
    editDir name = State (\(f, p, r) -> 
                    if R.inRoot r
                    then Left CannotEditRootDir
                    else Right ((), (F.editDir name f, F.editDirFromRoot name p r, R.editRoute r name)))
    taskInFolder name = State (\e@(f, p, r) -> Right (F.taskInFolder name f, e))
    folderInFolder name = State (\e@(f, p, r) -> Right (F.folderInFolder name f, e))
    folderInBackFolder name = State (\e@(f, p, r) -> Right (F.folderInFolderRoot name p (R.backRoute r), e))
    findFolder route = State (\e@(f, p, r) -> Right (F.findFolder route f, e))
    findBackFolder = State (\e@(f, p, r) -> Right (F.findBackFolder r p, e))
    search filter = State (\e@(f, p, r) -> do t <- F.search filter f
                                              return (t, e))
    searchR filter = State (\e@(f, p, r) -> do t <- F.searchRecursive filter p
                                               return (t, e))

instance MonadFail State where
    fail _ = State (\f -> Left WrongDateFormat)

instance MonadFail (Either Error) where
    fail _ = Left WrongDateFormat

instance MonadError State where
    throw e = State (\f -> Left e)

