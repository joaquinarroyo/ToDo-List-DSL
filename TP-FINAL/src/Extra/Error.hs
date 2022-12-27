module Extra.Error where

-- TODO: Agregar demas errores
data Error = DirNotFound String | TaskNotFound String | TaskAlreadyExists String deriving (Show, Eq)