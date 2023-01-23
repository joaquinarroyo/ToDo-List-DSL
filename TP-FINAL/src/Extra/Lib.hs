module Extra.Lib 
    (localTime,
     Extra.Lib.toLower,
     cast)
    where

import Data.Text as T (unpack, toLower, pack)
import Data.Time (parseTimeM, defaultTimeLocale)
import Structures.Task (Date (..))

-- Parsea el string a Date
localTime :: String -> Date
localTime s = case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" s of
                Nothing -> case parseTimeM True defaultTimeLocale "%Y-%m-%d" s of
                              Nothing -> Error
                              Just t -> T t
                Just t -> T t

-- Pasa el string a lowercase   
toLower :: String -> String
toLower s = T.unpack . T.toLower . T.pack $ s

-- Funcion para castear
cast :: (Show a, Read b) => a -> b
cast = read . show
