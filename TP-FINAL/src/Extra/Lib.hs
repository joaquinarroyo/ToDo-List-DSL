module Extra.Lib
    ( localTime
    ) where

import Data.Time
import Data.Time.Parsers hiding (localTime)

-- string to localTime
localTime :: (Monad m, MonadFail m) => String -> m LocalTime
localTime s = do t <- parseTimeM Prelude.True defaultTimeLocale "%Y-%m-%d %H:%M" s
                 return t
