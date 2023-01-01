module Command.AST where

import Structures.Task
import Structures.Route
import Filter.AST

data Command where
    NewTask :: Name -> Description -> Priority -> String -> Command
    DeleteTask :: String -> Command
    EditTask :: Name -> Field -> String -> Command
    EditTaskP :: Name -> Integer -> Command
    EditTaskT :: Name -> String -> Command
    EditTaskB :: Name -> Bool -> Command
    NewDir :: String -> Command
    DeleteDir :: String -> Command
    EditDir :: String -> Command
    LS :: Command
    CD :: Route -> Command
    Search :: Filter -> Bool -> Command
    Exit :: Command

deriving instance Show Command
deriving instance Eq Command
    