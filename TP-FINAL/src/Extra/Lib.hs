module Extra.Lib where

import qualified Data.Time as D
import qualified Data.Text as T
import Extra.Error
import Structures.Task (Date (..))

localTime :: (Monad m, MonadFail m) => String -> m Date
localTime "" = return Null
localTime s = case D.parseTimeM True D.defaultTimeLocale "%Y-%m-%d %H:%M" s of
                Nothing -> case D.parseTimeM True D.defaultTimeLocale "%Y-%m-%d" s of
                             Nothing -> fail ""
                             Just t -> return $ T t
                Just t -> return $ T t
                
toLower :: String -> String
toLower s = T.unpack . T.toLower . T.pack $ s
