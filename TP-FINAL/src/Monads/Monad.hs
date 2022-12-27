module Monads.Monad where
import Structures.Task
import Structures.Folder
import Extra.Error
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

newtype State a = State { runState :: Folder -> Either Error (Pair a Folder) }

class Monad m => MonadState m where
    get :: m ()
    -- goBack :: m ()
    -- goForward :: Name -> m ()

class Monad m => MonadError m where
    -- Lanza un error
    throw :: Error -> m a

instance Monad State where
    return x = State (\s -> Right (x :!: s))
    m >>= f = State (\s -> case runState m s of
                                Left e -> Left e
                                Right (v :!: s') -> runState (f v) s')

-- Para calmar al GHC
instance Functor State where
    fmap = liftM

instance Applicative State where
    pure  = return
    (<*>) = ap

instance MonadState State where
    get = State (\s -> Right (() :!: s))

instance MonadError State where
    throw e = State (\s -> Left e)





