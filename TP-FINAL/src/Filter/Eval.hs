module Filter.Eval
    (evalFilter)
    where

import Data.List (isInfixOf)
import Extra.Lib (localTime)
import Filter.AST (Filter (..))
import Structures.Task (Task (..), Field(..))

-- Evaluador de las expresiones de filtros sobre una tarea
evalFilter :: (MonadFail m) => Task -> Filter -> m Bool
evalFilter t (FieldEq Name s) = return $ tname t == s
evalFilter t (FieldEq Description s) = return $ description t == s
evalFilter t (FieldIlike Name s) = return $ isInfixOf s $ tname t
evalFilter t (FieldIlike Description s) = return $ isInfixOf s $ description t
evalFilter t (FieldEqB b) = return $ completed t == b
evalFilter t (FieldEqP p) = return $ priority t == p
evalFilter t (FieldEqT d) = do d' <- localTime d
                               return $ date t == d'
evalFilter t (FieldNEq Name s) = return $ tname t /= s
evalFilter t (FieldNEq Description s) = return $ description t /= s
evalFilter t (FieldNEqB b) = return $ completed t /= b
evalFilter t (FieldNEqP p) = return $ priority t /= p
evalFilter t (FieldNEqT d) = do d' <- localTime d
                                return $ date t /= d'
evalFilter t (FieldGtP p) = return $ priority t > p
evalFilter t (FieldLtP p) = return $ priority t < p
evalFilter t (FieldGteP p) = return $ priority t >= p
evalFilter t (FieldLteP p) = return $ priority t <= p
evalFilter t (FieldGtT d) = do d' <- localTime d
                               return $ date t > d'
evalFilter t (FieldLtT d) = do d' <- localTime d
                               return $ date t < d'
evalFilter t (FieldGteT d) = do d' <- localTime d
                                return $ date t >= d'
evalFilter t (FieldLteT d) = do d' <- localTime d
                                return $ date t <= d'
evalFilter t (And e1 e2) = do v1 <- evalFilter t e1 
                              v2 <- evalFilter t e2
                              return $ v1 && v2
evalFilter t (Or e1 e2) = do v1 <- evalFilter t e1 
                             v2 <- evalFilter t e2
                             return $ v1 || v2
evalFilter t (Not e) = do v <- evalFilter t e
                          return $ not v



