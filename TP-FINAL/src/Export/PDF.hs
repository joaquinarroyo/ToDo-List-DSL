module Export.PDF
  ( export
  ) where

import Data.Text
import Graphics.PDF
import Graphics.PDF.Fonts.StandardFont
import Prelude as P
import Structures.Task

-- Exporta las tareas a un archivo PDF
export :: [Task] -> IO ()
export ts = do
  f <- getFont Helvetica
  let rect = PDFRect 0 0 widht height
  runPdf
    route
    (standardDocInfo {author = toPDFString "System", compressed = False})
    rect $ do myDocument f ts widht (height - 30)
  where
    route = "exportedFiles/tasks.pdf"
    widht = 450
    defaultHeight = 700
    height' = fromIntegral $ 30 + (P.length ts) * 30
    height =
      if height' > defaultHeight
        then height'
        else defaultHeight

-- Crea el documento PDF
myDocument :: AnyFont -> [Task] -> Double -> Double -> PDF ()
myDocument _ [] _ _ = return ()
myDocument f ts w h = do
  page <- addPage Nothing
  drawWithPage page $ do
    drawText $ do
      setFont (PDFFont f 15)
      textStart ((w / 2) - 45) h
      renderMode FillText
      displayText $ toPDFString "Tasks"
  newSection (toPDFString "Tasks") Nothing Nothing $ do
    createPageContent f ts (h - 20) page

-- Crea el contenido de la pagina
createPageContent ::
     AnyFont -> [Task] -> Double -> PDFReference PDFPage -> PDF ()
createPageContent _ [] _ _ = return ()
createPageContent f (t:ts) h page = do
  createPageContent' f t h page
  createPageContent f ts (h - 15) page

-- FUncion auxiliar que va creando el contenido de la pagina
createPageContent' ::
     AnyFont -> Task -> Double -> PDFReference PDFPage -> PDF ()
createPageContent' f t h page =
  drawWithPage page $ do
    drawText $ do
      setFont (PDFFont f 12)
      textStart 10 h
      renderMode FillText
      displayText $ toPDFString ((formatTask t))

-- Formatea la tarea para mostrarla en el PDF
formatTask :: Task -> String
formatTask t =
  (tname t) ++
  ": " ++
  (description t) ++
  ", " ++
  (show $ priority t) ++ ", " ++ (show $ date t) ++ " " ++ (show' $ completed t)
  where
    show' True = "✓"
    show' False = "X"

-- Funcion para obtener la fuente
getFont :: FontName -> IO AnyFont
getFont fontName = do
  font <- mkStdFont fontName
  case font of
    Right f -> return f
    Left _ -> error "Error al cargar la fuente"

-- Funcion para convertir un String a DataTextText
toPDFString :: String -> Data.Text.Text
toPDFString s = Data.Text.pack s
