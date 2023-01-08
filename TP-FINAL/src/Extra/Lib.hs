module Extra.Lib 
    (localTime,
     Extra.Lib.toLower)
    where

import Data.Text as T (unpack, toLower, pack)
import Data.Time (parseTimeM, defaultTimeLocale)
import Structures.Task (Date (..))

-- Parsea un String y lo pasa a Date
localTime :: (MonadFail m) => String -> m Date
localTime "" = return Null
localTime s = case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" s of
                Nothing -> case parseTimeM True defaultTimeLocale "%Y-%m-%d" s of
                             Nothing -> fail ""
                             Just t -> return $ T t
                Just t -> return $ T t

-- Pasa el string a lowercase   
toLower :: String -> String
toLower s = T.unpack . T.toLower . T.pack $ s
