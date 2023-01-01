module Eval.EvalFilter where

import Filter.AST
import Structures.Task
import Extra.Lib
import Extra.Error
import Data.Time
import Data.List

-- Evaluador de las expresiones de filtros sobre una tarea
evalFilter :: (Monad m, MonadFail m) => Task -> Filter -> m Bool
evalFilter t (FieldEq Name s) = return $ getTaskName t == s
evalFilter t (FieldEq Description s) = return $ getTaskDescription t == s
evalFilter t (FieldIlike Name s) = return $ isInfixOf s $ getTaskName t
evalFilter t (FieldIlike Description s) = return $ isInfixOf s $ getTaskDescription t
evalFilter t (FieldEqB b) = return $ isTaskCompleted t == b
evalFilter t (FieldEqP p) = return $ getTaskPriority t == p
evalFilter t (FieldEqT d) = do d' <- localTime d
                               return $ getTaskTimestamp t == d'
evalFilter t (FieldNEq Name s) = return $ getTaskName t /= s
evalFilter t (FieldNEq Description s) = return $ getTaskDescription t /= s
evalFilter t (FieldNEqB b) = return $ isTaskCompleted t /= b
evalFilter t (FieldNEqP p) = return $ getTaskPriority t /= p
evalFilter t (FieldNEqT d) = do d' <- localTime d
                                return $ getTaskTimestamp t /= d'
evalFilter t (FieldGtP p) = return $ getTaskPriority t > p
evalFilter t (FieldLtP p) = return $ getTaskPriority t < p
evalFilter t (FieldGteP p) = return $ getTaskPriority t >= p
evalFilter t (FieldLteP p) = return $ getTaskPriority t <= p
evalFilter t (FieldGtT d) = do d' <- localTime d
                               return $ getTaskTimestamp t > d'
evalFilter t (FieldLtT d) = do d' <- localTime d
                               return $ getTaskTimestamp t < d'
evalFilter t (FieldGteT d) = do d' <- localTime d
                                return $ getTaskTimestamp t >= d'
evalFilter t (FieldLteT d) = do d' <- localTime d
                                return $ getTaskTimestamp t <= d'
evalFilter t (And e1 e2) = do v1 <- evalFilter t e1 
                              v2 <- evalFilter t e2
                              return $ v1 && v2
evalFilter t (Or e1 e2) = do v1 <- evalFilter t e1 
                             v2 <- evalFilter t e2
                             return $ v1 || v2
evalFilter t (Not e) = do v <- evalFilter t e
                          return $ not v



