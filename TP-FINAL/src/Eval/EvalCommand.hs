module Eval.EvalCommand where

import Command.AST
import Monads.Monad
import Structures.Route (Route (..))
import Structures.Env (Env)
import Structures.Task (Task (..), Field (..))
import Extra.Lib (localTime)
import Extra.Error

-- Evaluador de comandos
eval :: Command -> Env -> Either Error (Env, [Task])
eval c env = case runState (eval' c) env of
                Left e -> Left e
                Right (t, f) -> Right (f, t)

eval' :: (MonadState m, MonadError m) => Command -> m [Task]
eval' (NewTask n d p t) = do b <- taskInFolder n
                             if b
                             then throw $ TaskAlreadyExists n
                             else do t' <- localTime t
                                     addTask n d p t'
                                     return []
eval' (DeleteTask s) = do deleteTask s
                          return []
eval' (EditTask n f@Name s) = do b <- taskInFolder n
                                 if not b
                                 then throw $ TaskNotFound s
                                 else do b' <- taskInFolder s
                                         if b'
                                         then throw $ TaskAlreadyExists s
                                         else do editTask n f s
                                                 return []
eval' (EditTask n f@Description s) = do b <- taskInFolder n
                                        if not b
                                        then throw $ TaskNotFound n
                                        else do editTask n f s
                                                return []
eval' (EditTaskP n p) = do b <- taskInFolder n
                           if not b
                           then throw $ TaskNotFound n
                           else do editTaskP n p
                                   return []
eval' (EditTaskT n t) = do b <- taskInFolder n
                           if not b
                           then throw $ TaskNotFound n
                           else do t' <- localTime t
                                   editTaskT n t'
                                   return []
eval' (EditTaskB n b) = do b' <- taskInFolder n
                           if not b'
                           then throw $ TaskNotFound n
                           else do editTaskB n b
                                   return []
eval' (NewDir s) = do b <- folderInFolder s
                      if b
                      then throw $ DirAlreadyExists s
                      else do addDir s
                              return []
eval' (DeleteDir s) = do deleteDir s
                         return []
eval' (EditDir s) = do b <- folderInBackFolder s
                       if b
                       then throw $ DirAlreadyExists s
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
                   Nothing -> throw $ DirNotFound (show r)
                   Just f' -> do setFolder f'
                                 setRoute r
                                 return []
eval' (Search f False) = do t <- search f
                            return t
eval' (Search f True) = do t <- searchR f
                           return t

                            