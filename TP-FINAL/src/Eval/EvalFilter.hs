module Eval.EvalFilter where

import Filter.AST
import Structures.Task
import Data.Time
import Data.Fixed
import Extra.Lib
                 
-- Evaluador de las expresiones de filtros sobre una tarea
evalFilter :: (Monad m, MonadFail m) => Task -> Filter -> m Bool
evalFilter t (FieldEq Name s) = return (getTaskName t == s)
evalFilter t (FieldEq Description s) = return (getTaskDescription t == s)
evalFilter t (FieldEqB s) = return (isTaskCompleted t == s)
evalFilter t (FieldEqT s) = do d <- localTime s
                               return $ getTaskTimestamp t == d
evalFilter t (FieldEqP s) = return (getTaskPriority t == s)
evalFilter t (FieldGtP s) = return (getTaskPriority t > s)
evalFilter t (FieldGtT s) = do d <- localTime s
                               return $ getTaskTimestamp t > d
evalFilter t (FieldLtP s) = return (getTaskPriority t < s)
evalFilter t (FieldLtT s) = do d <- localTime s
                               return $ getTaskTimestamp t < d
evalFilter t (And e1 e2) = do v1 <- evalFilter t e1 
                              v2 <- evalFilter t e2
                              return (v1 && v2)
evalFilter t (Or e1 e2) = do v1 <- evalFilter t e1 
                             v2 <- evalFilter t e2
                             return (v1 || v2)
evalFilter t (Not e) = do v <- evalFilter t e
                          return (not v)




