module Command.Eval
        (eval) 
        where

import Command.AST (Command (..))
import Extra.Error (Error (..))
import Monad.Env (Env)
import Monad.State (State(runState), MonadState (..), MonadError (..))
import Structures.Task (Task (..), Field (..), Date(..), Name)
import Structures.Route (Route (..))

-- Evaluador de comandos
eval :: Command -> Env -> Either Error (Env, [Task])
eval c env = case runState (eval' c) env of
                Left e -> Left e
                Right (t, f) -> Right (f, t)

eval' :: (MonadState m, MonadError m) => Command -> m [Task]
eval' (NewTask n d p t) = do b <- taskInFolder n
                             if b
                             then throw TaskAlreadyExists
                             else case t of
                                   Error -> throw WrongDateFormat
                                   _ -> do addTask n d p t
                                           return []
eval' (DeleteTask s) = do deleteTask s
                          return []
eval' (EditTaskName n s) = do b <- taskInFolder n
                              if not b
                              then throw TaskNotFound
                              else do b' <- taskInFolder (show s)
                                      if b'
                                      then throw TaskAlreadyExists
                                      else do editTask n Name s
                                              return []
eval' (EditTaskTimestamp n t) = do b <- taskInFolder n
                                   if not b
                                   then throw TaskNotFound
                                   else case t of
                                         Error -> throw WrongDateFormat
                                         _ -> do editTask n Timestamp t
                                                 return []
eval' (EditTaskDescription n s) = editTaskAux n Description s
eval' (EditTaskPriority n p) = editTaskAux n Priority p
eval' (EditTaskCompleted n b) = editTaskAux n Completed b
eval' (NewDir s) = do b <- folderInFolder s
                      if b
                      then throw DirAlreadyExists
                      else do addDir s
                              return []
eval' (DeleteDir s) = do deleteDir s
                         return []
eval' (EditDir s) = do b <- folderInParent s
                       if b
                       then throw DirAlreadyExists
                       else do editDir s
                               return []
eval' (CD Back) = do f <- findBackFolder
                     case f of   
                      Just f' -> do setFolder f'
                                    backRoute
                                    return []
                      _ -> return []
eval' (CD r) = do f <- findFolder r
                  case f of
                   Nothing -> throw DirNotFound
                   Just f' -> do setFolder f'
                                 setRoute r
                                 return []
eval' (Search f False) = do l <- search f
                            return l
eval' (Search f True) = do l <- searchR f
                           return l

-- Funcion auxiliar para editar tareas
editTaskAux :: (MonadState m, MonadError m, Read a, Show a) => Name -> Field -> a -> m [Task]
editTaskAux n f a = do b <- taskInFolder n
                       if not b
                        then throw TaskNotFound
                        else do editTask n f a
                                return []

                            