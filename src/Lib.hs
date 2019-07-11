{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map              as M

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- Data
data Pos = Pos Int Int
  deriving (Eq, Ord)

type Dim = Pos

data Dir = Left | Up | Right | Down
  deriving (Eq, Ord, Enum, Bounded)

data Variant = Spinner Dir
             | Walker Pos Pos
  deriving Eq

data Event = ChangeDir Pos Dir
           | Move Pos Dir
           | Shoot Pos
  deriving (Eq, Ord)




data Entity = Entity { _life :: Integer
                     , _dir  :: Dir
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



data Game = Cont Game | Ended TEnv


-- Combinators

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
enemyStep :: Monad m => StepT m (M.Map Pos TEnemy)
enemyStep = undefined -- logic

-- internal
playerStep :: Monad m => Event -> StepT m (Pos, TPlayer)
playerStep event = ((,) event <$>) <$> player'

-- exported to user to create (StepT m TEnv)
transStep :: Monad m => Event -> StepT m TEnv
transStep e = TEnv <$> dim' <*> obstacles' <*> enemyStep <*> playerStep e

-- exported
mkGame :: Monad m => StepT m TEnv -> Env -> m Game
mkGame step = runStepT go
  where go = do tenv <- step
                case resolve tenv of
                  Just env -> Cont <$> lift (mkGame step env)
                  Nothing  -> return (Ended tenv)

resolve :: TEnv -> Maybe Env
resolve = undefined



-- Game Interpreter
runGame :: Game -> IO ()
runGame (Cont g)  = runGame g
runGame (Ended _) = putStrLn "Ended"
