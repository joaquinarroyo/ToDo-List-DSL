module Profile.Profile 
        ( ProfileName,
          firstLoad,
          newProfile,
          deleteProfile,
          saveProfile,
          loadProfile,
          lastProfileName
        ) where

import Control.Monad.Except (lift)
import Data.Aeson
import Extra.Error (Error ( ProfileAlreadyExists, 
                            ProfileDoesNotExists, 
                            ErrorLoadingProfile, 
                            CannotDeleteDefaultProfile))
import Extra.Pp (printError, printMessage)
import Monad.Env (Env)
import Structures.Folder (Folder, newdir)
import Structures.Task (Task, Date)
import System.Console.Haskeline (InputT, outputStrLn)
import System.Directory (doesFileExist, removeFile)

type ProfileName = String

-- Instancias para formatear/desformatear de .json
instance ToJSON Folder where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Date where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Task where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Folder where
    parseJSON = genericParseJSON defaultOptions
instance FromJSON Date where
    parseJSON = genericParseJSON defaultOptions
instance FromJSON Task where
    parseJSON = genericParseJSON defaultOptions

-- Crea/Carga el perfil default
firstLoad :: ProfileName -> InputT IO (Maybe Folder)
firstLoad pn = do b <- lift $ doesFileExist $ route pn
                  if b
                  then do file <- lift $ readFile $ route pn
                          return $ decode $ read file
                  else do lift $ writeFile (route pn) $ show (encode $ dir)
                          return $ Just dir
    where dir = newdir "root"

-- Crea un nuevo perfil con el nombre recibido
newProfile :: ProfileName -> InputT IO ()
newProfile pn = do b <- lift $ doesFileExist $ route pn
                   if b
                   then outputStrLn $ (printError $ ProfileAlreadyExists pn)
                   else do outputStrLn $ printMessage "Creating" pn
                           lift $ writeFile (route pn) $ show (encode $ newdir "root")

-- Guarda el perfil en un archivo <pn>.json
saveProfile :: ProfileName -> Env -> InputT IO ()
saveProfile pn (_, f, _) = do outputStrLn $ printMessage "Saving" pn
                              lift $ writeFile (route pn) $ show (encode f)

-- Carga el perfil desde un archivo .json a partir del nombre recibido
loadProfile :: ProfileName -> InputT IO (Maybe Folder)
loadProfile pn = do b <- lift $ doesFileExist $ route pn
                    if b
                    then do outputStrLn $ printMessage "Loading" pn
                            lift $ writeFile lastProfile $ pn
                            file <- lift $ readFile $ route pn
                            case decode $ read file of
                                Just f -> return $ Just f
                                _ -> do outputStrLn (printError $ ErrorLoadingProfile pn)
                                        return Nothing
                    else do outputStrLn (printError $ ProfileDoesNotExists pn)
                            return Nothing

-- Borra el perfil a partir de su nombre
deleteProfile :: ProfileName -> InputT IO Bool
deleteProfile pn = if pn == "default"
                   then do outputStrLn $ printError CannotDeleteDefaultProfile
                           return False
                   else do outputStrLn $ printMessage "Deleting" pn
                           lift $ removeFile (route pn)
                           return True 

-- Devuelve el nombre del ultimo perfil cargado
lastProfileName :: IO String
lastProfileName = do c <- readFile lastProfile
                     return $ c

-- Last profile file name
lastProfile :: String
lastProfile = "profiles/lastprofile.txt"

-- Profiles route
route :: String -> String
route s = "profiles/" ++ s ++ ".json"

