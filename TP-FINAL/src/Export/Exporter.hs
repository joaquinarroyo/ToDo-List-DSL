module Export.Exporter
  ( Export.Exporter.export
  , FileType(..))
  where

import Export.PDF as P
import Structures.Task

data FileType = PDF
  deriving (Eq, Show)

-- Funcion que exporta las tareas a un archivo segun el tipo recibido
export :: FileType -> String -> [Task] -> IO ()
export PDF folderName ts = P.export folderName ts
