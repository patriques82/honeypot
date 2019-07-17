{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map              as M

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- Data Types
data Pos = Pos Int Int
  deriving (Eq, Ord)

type Dim = Pos

data Dir = Left | Up | Right | Down
  deriving (Eq, Ord, Enum, Bounded)

data Variant = Spinner Dir
             | Walker Pos Pos
  deriving Eq

data Event = ChangeDir Dir
           | Move Dir
           | Shoot
  deriving (Eq, Ord)




data Entity = Entity { _life     :: Integer
                     , _dir      :: Dir
                     , _shooting :: Bool
                     } deriving Eq

data Enemy = Enemy Entity Variant
  deriving Eq

type TEnemy = (Event, Enemy)

newtype Player = Player Entity

type TPlayer = (Event, Player)



data Env = Env { _dim       :: Dim
               , _obstacles :: [Pos]
               , _enemies   :: M.Map Pos Enemy
               , _player    :: (Pos, Player)
               }

data TEnv = TEnv Dim [Pos] (M.Map Pos TEnemy) (Pos, TPlayer)



-- Step Combinators

-- Similar to a State Monad with the difference that the state change is withheld
-- until the total step is runned so that intermediate steps doesnt know the
-- state changes that are occuring during the execution of the total step.

newtype StepT m a = StepT (ReaderT Env m a) -- hide constructor on export
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadTrans)

type Step a = StepT Identity a

runStepT :: Monad m => StepT m a -> Env -> m a
runStepT (StepT step) = runReaderT step

runStep :: Step a -> Env -> a
runStep step = runIdentity . runStepT step



env' :: Monad m => StepT m Env
env' = StepT ask

dim' :: Monad m => StepT m Dim
dim' = _dim <$> env'

obstacles' :: Monad m => StepT m [Pos]
obstacles' = _obstacles <$> env'

-- TODO List of (Pos, Enemy)
enemies' :: Monad m => StepT m (M.Map Pos Enemy)
enemies' = _enemies <$> env'

player' :: Monad m => StepT m (Pos, Player)
player' = _player <$> env'


-- internal
-- TODO parts of enemystep subsystem
enemyStep :: Monad m => StepT m (M.Map Pos TEnemy)
enemyStep = undefined -- logic

-- internal
playerStep :: Monad m => Event -> StepT m (Pos, TPlayer)
playerStep event = ((,) event <$>) <$> player'

-- exported to user to create (StepT m TEnv)
transStep :: Monad m => Event -> StepT m TEnv
transStep e = TEnv <$> dim' <*> obstacles' <*> enemyStep <*> playerStep e

-- exported
frames :: Monad m => StepT m TEnv -> Env -> m [Env]
frames step env = do tenv <- runStepT step env
                     case resolve tenv of
                       Just e  -> fmap ((:) e) (frames step e)
                       Nothing -> return []



-- TODO parts of resolve subsystem
resolve :: TEnv -> Maybe Env
resolve tenv@(TEnv d o _ _) = do
  e' <- resolveEnemies tenv
  p' <- resolvePlayer tenv
  return (Env d o e' p')

resolveEnemies :: TEnv -> Maybe (M.Map Pos Enemy)
resolveEnemies = undefined

resolvePlayer :: TEnv -> Maybe (Pos, Player)
resolvePlayer = undefined



