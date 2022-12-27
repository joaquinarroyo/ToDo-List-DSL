module Structures.Task where
import Data.Time

type Name = String
type Description = String
type Completed = Bool
type Priority = Integer
type Timestamp = LocalTime

-- Task (Name, Description, Date, Priority)
data Task = Task Name Description Completed Priority Timestamp deriving (Show, Eq)
data Field = Name | Description | Completed | Timestamp | Priority deriving (Show, Eq)

-- Devuelve el nombre de la tarea recibida
getTaskName :: Task -> Name
getTaskName (Task n _ _ _ _) = n

-- Devuelve la descripcion de la tarea recibida
getTaskDescription :: Task -> Description
getTaskDescription (Task _ d _ _ _) = d

-- Devuelve el estado de la tarea recibida
isTaskCompleted :: Task -> Completed
isTaskCompleted (Task _ _ c _ _) = c

-- Devuelve la prioridad de la tarea recibida
getTaskPriority :: Task -> Priority
getTaskPriority (Task _ _ _ p _) = p

-- Devuelve la fecha limite de la tarea recibida
getTaskTimestamp :: Task -> Timestamp
getTaskTimestamp (Task _ _ _ _ t) = t

----------------------------------- TASK COMMANDS ---------------------------------------------------------

-- Crea una nueva tarea con los datos recibidos
newTask :: (Monad m) => Name -> Description -> Priority -> Timestamp -> m Task
-- TODO: hacer chequeo sobre la fecha
newTask n d p t = return (Task n d False p t)

-- Edita el field de la tarea recibida
editTask :: Task -> Field -> String -> Task
editTask (Task _ d c p t) Name s = Task s d c p t
editTask (Task n _ c p t) Description s = Task n s c p t
editTask (Task n d _ p t) Completed s = Task n d (read s) p t
editTask (Task n d c _ t) Priority s = Task n d c (read s) t
editTask (Task n d c p _) Timestamp s = Task n d c p (read s)

-- Completa la tarea recibida
completeTask :: Task -> Task
completeTask (Task n d _ p t) = Task n d True p t



