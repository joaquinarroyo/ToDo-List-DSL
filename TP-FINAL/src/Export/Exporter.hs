module Export.Exporter 
  ( Export.Exporter.export
  , FileType(..))
  where

import Export.PDF as P
import Structures.Task

type Message = String
data FileType = PDF
  deriving (Eq, Show)

-- Funcion que exporta las tareas a un archivo segun el tipo recibido
export :: FileType -> [Task] -> IO ()
export PDF ts = P.export ts
