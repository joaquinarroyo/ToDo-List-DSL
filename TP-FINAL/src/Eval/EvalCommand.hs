module Eval.EvalCommand where

import Command.AST
import Eval.EvalFilter
import Monads.Monad
import Structures.Folder
import Structures.Task
import Extra.Lib
import Extra.Error

-- eval :: (MonadState m, MonadError m) => Command -> m ()
-- eval (NewTask n d p s) = State (\f -> if taskInFolder s f
--                                       then throw (TaskAlreadyExists s)
--                                       else do t <- localTime s
--                                               task <- newTask n d p t
--                                               return ((), addTask task f))
-- eval (DeleteTask s) = State (\f -> do t <- findTask s f
--                                       case t of
--                                         Nothing -> throw (TaskNotFound s)
--                                         Just t -> return ((), Folder (getFolderName f) (deleteTask s (getFolderItems f))))
-- eval (EditTask f s) = undefined
-- eval (EditTaskP p) = undefined
-- eval (EditTaskT t) = undefined
-- eval (EditTaskB b) = undefined
-- eval (NewDir s) = undefined
-- eval (DeleteDir s) = undefined
-- eval (EditDir s) = undefined
-- eval LS = undefined
-- eval (CD r) = undefined
-- eval (Search f) = undefined


                            