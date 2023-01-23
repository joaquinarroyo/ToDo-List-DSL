module Filter.AST 
    (Filter (..))
    where

import Structures.Task (Field (..), Date (..))

-- Estructura para las expresiones del filtro de tareas
data Filter where
    FieldEq :: Field -> String -> Filter
    FieldNEq :: Field -> String -> Filter
    FieldIlike :: Field -> String -> Filter
    FieldNEqB :: Bool -> Filter
    FieldEqB :: Bool -> Filter
    FieldEqP :: Integer -> Filter
    FieldNEqP :: Integer -> Filter
    FieldGtP :: Integer -> Filter
    FieldLtP :: Integer -> Filter
    FieldGteP :: Integer -> Filter
    FieldLteP :: Integer -> Filter
    FieldEqT :: Date -> Filter
    FieldNEqT :: Date -> Filter
    FieldGtT :: Date -> Filter
    FieldLtT :: Date -> Filter
    FieldGteT :: Date -> Filter
    FieldLteT :: Date -> Filter
    And :: Filter -> Filter -> Filter
    Or :: Filter -> Filter -> Filter
    Not :: Filter -> Filter

deriving instance Eq Filter

