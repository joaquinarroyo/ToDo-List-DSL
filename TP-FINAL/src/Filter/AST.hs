module Filter.AST where
import Structures.Task

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
    FieldEqT :: String -> Filter
    FieldNEqT :: String -> Filter
    FieldGtT :: String -> Filter
    FieldLtT :: String -> Filter
    FieldGteT :: String -> Filter
    FieldLteT :: String -> Filter
    And :: Filter -> Filter -> Filter
    Or :: Filter -> Filter -> Filter
    Not :: Filter -> Filter

deriving instance Show Filter
deriving instance Eq Filter
