module Command.AST 
    (Command (..))
    where

import Filter.AST (Filter (..))
import Structures.Route (Route (..))
import Structures.Task (Date(..), Name, Description, Priority)

-- -- Estructura para los comandos
data Command where
    NewTask :: Name -> Description -> Priority -> Date -> Command
    DeleteTask :: String -> Command
    EditTaskName :: Name -> String -> Command
    EditTaskDescription :: Name -> String -> Command
    EditTaskPriority :: Name -> Integer -> Command
    EditTaskTimestamp :: Name -> Date -> Command
    EditTaskCompleted :: Name -> Bool -> Command
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