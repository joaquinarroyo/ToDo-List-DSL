module Command.AST 
    (Command (..))
    where

import Filter.AST (Filter (..))
import Structures.Route (Route (..))
import Structures.Task (Field(..), Name, Description, Priority)

-- Estructura para los comandos
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
    Help :: Command
    LoadProfile :: String -> Command
    SaveProfile :: Command
    NewProfile :: String -> Command
    DeleteProfile :: Command
    ShowProfiles :: Command

deriving instance Eq Command
    