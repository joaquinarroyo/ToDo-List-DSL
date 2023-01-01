module Structures.Task where
import Data.Time

-- Sinonimos de tipos
type Name = String
type Description = String
type Completed = Bool
type Priority = Integer

data Date = Null | T LocalTime deriving (Eq) 

instance Show Date where
    show (T t) = show t
    show Null = ""

instance Ord Date where
    compare Null Null = EQ
    compare Null _ = LT
    compare _ Null = GT
    compare (T t1) (T t2) = compare t1 t2

-- (Name, Description, Completed, Date, Priority)
data Task = Task Name Description Completed Priority Date deriving (Eq)

instance Show Task where
    show (Task n d c p t) = n ++ " | " ++ d ++ " | " ++ show c ++ " | " ++ show p ++ " | " ++ show t

instance Ord Task where
    compare (Task _ _ _ p1 t1) (Task _ _ _ p2 t2) = if p1 == p2
                                                    then compare t1 t2
                                                    else compare p1 p2

-- Fields de Task
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
getTaskTimestamp :: Task -> Date
getTaskTimestamp (Task _ _ _ _ t) = t

-- Crea una nueva tarea con los datos recibidos
newTask :: Name -> Description -> Priority -> Date -> Task
newTask n d p t = Task n d False p t

-- Edita el field de la tarea recibida
-- Solo utiliza Field = Name o Field = Description
editTask :: Task -> Field -> String -> Task
editTask (Task _ d c p t) Name s = Task s d c p t
editTask (Task n _ c p t) Description s = Task n s c p t

-- Analogo a editTask pero para completed
editTaskB :: Task -> Completed -> Task
editTaskB (Task n d _ p t) b = Task n d b p t

-- Analogo a editTask pero para priority
editTaskP :: Task -> Priority -> Task
editTaskP (Task n d c _ t) p = Task n d c p t

-- Analogo a editTask pero para timestamp
editTaskT :: Task -> Date -> Task
editTaskT (Task n d c p _) t = Task n d c p t

-- Completa la tarea recibida
completeTask :: Task -> Task
completeTask (Task n d _ p t) = Task n d True p t

-- Busca una tarea a partir de su nombre
findTask :: Name -> [Task] -> Maybe Task
findTask n [] = Nothing
findTask n (t:ts) = if n == getTaskName t
                    then Just t
                    else findTask n ts

-- Borra una tarea a partir de su nombre
deleteTask :: Name -> [Task] -> [Task]
deleteTask n [] = []
deleteTask n (t:ts) = if n == getTaskName t
                      then deleteTask n ts 
                      else t : deleteTask n ts

-- Formatea una lista de tareas para mostrarlas por pantalla
showTasks :: [Task] -> String
showTasks l = "Name | Description | Completed | Priority | Timestamp" ++ "\n" ++ showTasks' l

showTasks' :: [Task] -> String
showTasks' [] = ""
showTasks' (t:[]) = show t
showTasks' (t:ts) = show t ++ "\n" ++ showTasks' ts

