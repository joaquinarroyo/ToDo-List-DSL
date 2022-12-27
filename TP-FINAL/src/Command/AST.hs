module Command.AST where

import Structures.Task
import Structures.Folder
import Filter.AST

data Command where
    NewTask :: Name -> Description -> Priority -> String -> Command
    DeleteTask :: String -> Command
    EditTask :: Field -> String -> Command
    EditTaskP :: Integer -> Command
    EditTaskT :: String -> Command
    EditTaskB :: Bool -> Command
    NewDir :: String -> Command
    DeleteDir :: String -> Command
    EditDir :: String -> Command
    LS :: Command
    CD :: Route -> Command
    Search :: Filter -> Command

deriving instance Show Command
deriving instance Eq Command
    