module Structures.Folder where
import Structures.Task
import Filter.AST

data Route = Root | Empty | Back | Route Name Route deriving (Show, Eq)
data Folder = Folder Name [Folder] | T Task deriving (Show, Eq)

-- Devuelve el nombre de la carpeta recibida
getFolderName :: Folder -> Name
getFolderName (Folder n _) = n

getFolderItems :: Folder -> [Folder]
getFolderItems (Folder _ fs) = fs

----------------------------------- FOLDER COMMANDS ---------------------------------------------------------

-- Busca todas las tareas que cumplan con el filtro recibido, a partir de una carpeta, recursivamente hacia
-- subcarpetas.
-- search :: Folder -> Filter -> [Task]
-- search (Folder _ []) _ = []
-- search (Folder _ ((T task):xs)) f = if evalFilter task f 
--                                     then task : search (Folder "" xs) f 
--                                     else search (Folder "" xs) f
-- search (Folder _ ((F folder):xs)) f = search folder f ++ search (Folder "" xs) f

-- Crea una nueva carpeta con el nombre recibido
newdir :: Name -> Folder
newdir n = Folder n []

addTask :: Task -> Folder -> Folder
addTask t (Folder n fs) = Folder n (T t : fs)

addDir :: Folder -> Folder -> Folder
addDir f (Folder n fs) = Folder n (f : fs)

findTask :: Name -> [Folder] -> Maybe Task
findTask n [] = Nothing
findTask n ((T task):xs) = if n == getTaskName task 
                           then Just task 
                           else findTask n xs
findTask n (x:xs) = findTask n xs

taskInFolder :: Name -> Folder -> Bool
taskInFolder n (Folder _ fs) = case findTask n fs of
                                    Just _ -> True
                                    Nothing -> False

deleteTask :: Name -> [Folder] -> [Folder]
deleteTask n [] = []
deleteTask n ((T task):xs) = if n == getTaskName task 
                             then deleteTask n xs 
                             else (T task) : deleteTask n xs
deleteTask n (x:xs) = x : deleteTask n xs


-- Edita el nombre de la carpeta recibida
-- editdir :: Name -> Folder -> Folder
-- editdir n (Folder _ fs) = Folder n fs

-- Borra una carpeta a partir de su nombre
-- deletedir :: Name -> Folder -> Folder
-- -- Enviar mensaje de error si no existe la carpeta
-- deletedir n (Folder n' []) = Folder n' [] 
-- deletedir n (Folder n' (x:xs)) = if n == n' 
--                                  then Folder n' xs 
--                                  else Folder n' (x : deletedir n (Folder "" xs))

-- cd command
-- cd :: Route -> Folder -> Folder
-- cd Root f = f
-- -- ver como implementar esto
-- cd Empty f = f
-- cd Back (Folder n fs) = Folder n fs
-- cd (Route n r) f = case findFolder n f of
--                         Just f' -> cd r f'
--                         -- Enviar mensaje de error si no existe la ruta
--                         Nothing -> f

-- ls command
-- Dividimos entre tareas y carpetas para que sea mas comodo trabajar con la info.
-- TODO: Implementar banderas
ls :: Folder -> ([Name], [Task])
ls (Folder _ []) = ([], [])
ls (Folder _ (x:xs)) = case x of
                        Folder n _ -> (n : ns, ts)
                        T t ->        (ns, t : ts)
                        where (ns, ts) = ls (Folder "" xs)


