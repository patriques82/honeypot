{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

--import           Control.Monad
import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map              as M
--import           Prelude               as P


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




data Entity = Entity { life :: Integer
                     , dir  :: Dir
                     } deriving Eq

data Enemy = Enemy Entity Variant
  deriving Eq

type TEnemy = (Event, Enemy)

newtype Player = Player Entity

type TPlayer = (Event, Player)



data Env = Env { dim       :: Dim
               , obstacles :: [Pos]
               , enemies   :: M.Map Pos Enemy
               , player    :: (Pos, Player)
               }

data TEnv = TEnv Dim [Pos] (M.Map Pos TEnemy) (Pos, TPlayer)


-- Combinators

-- Similar to a State Monad with the difference that the state change is withheld
-- until the total step is runned so that intermediate steps doesnt know the
-- state changes that are occuring during the execution of the total step.
newtype StepT m a = StepT (ReaderT Env m a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

type Step a = StepT Identity a

type PlayerEvent m = StepT m Event

env :: Monad m => StepT m Env
env = StepT ask

dim' :: Monad m => StepT m Dim
dim' = dim <$> env

obstacles' :: Monad m => StepT m [Pos]
obstacles' = obstacles <$> env

enemies' :: Monad m => StepT m (M.Map Pos Enemy)
enemies' = enemies <$> env

player' :: Monad m => StepT m (Pos, Player)
player' = player <$> env



runStepT :: Monad m => StepT m a -> Env -> m a
runStepT (StepT step) = runReaderT step

runStep :: Step a -> Env -> a
runStep step = runIdentity . runStepT step



enemyStep :: Monad m => StepT m (M.Map Pos TEnemy)
enemyStep = undefined

playerStep :: Monad m => Event -> StepT m (Pos, TPlayer)
playerStep event = ((,) event <$>) <$> player'

transStep :: Monad m => PlayerEvent m -> StepT m TEnv
transStep playerEvent =
  TEnv <$> dim' <*> obstacles' <*> enemyStep <*> (playerEvent >>= playerStep)


-- Util
iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
iterateUntilM p f v
  | p v       = return ()
  | otherwise = f v >>= iterateUntilM p f


-- Run
ended :: a -> Bool
ended = undefined

-- Game Loop
runGame :: Monad m => StepT m a -> Env -> m ()
runGame step = iterateUntilM ended exec
  where exec = runStepT (step >> env)

