module Filter.AST where
import Structures.Task

data Filter where
    -- TODO: Ver como manejar esto
    FieldEq :: Field -> String -> Filter
    FieldEqB :: Bool -> Filter
    FieldEqP :: Integer -> Filter
    FieldEqT :: String -> Filter
    FieldGtP :: Integer -> Filter
    FieldLtP :: Integer -> Filter
    FieldGtT :: String -> Filter
    FieldLtT :: String -> Filter
    And :: Filter -> Filter -> Filter
    Or :: Filter -> Filter -> Filter
    Not :: Filter -> Filter

deriving instance Show Filter
deriving instance Eq Filter
