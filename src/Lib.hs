{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map              as M
import           Prelude               as P


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



data Entity = Entity Integer Dir
  deriving Eq

data Enemy = Enemy Entity Variant
  deriving Eq

type TEnemy = (Event, Enemy)

newtype Player = Player Entity

type TPlayer = (Event, Player)



data Env = Env { obstacles :: [Pos]
               , enemies   :: M.Map Pos Enemy
               , player    :: (Pos, Player)
               , dim       :: Dim
               }

data TEnv = TEnv [Pos] (M.Map Pos TEnemy) (Pos, TPlayer) Dim

data Event = ChangeDir Pos Dir
           | Move Pos Dir
           | Shoot Pos
  deriving (Eq, Ord)


-- Combinators

-- Similar to a State Monad with the difference that the state change is withheld
-- until the total step is runned so that intermediate steps doesnt know the
-- state changes that are occuring during the execution of the total step.
newtype StepT m a = StepT (ReaderT Env m a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

type Step a = StepT Identity a

env :: Monad m => StepT m Env
env = StepT ask

runStepT :: Monad m => StepT m a -> Env -> m a
runStepT (StepT step) = runReaderT step

runStep :: Step a -> Env -> a
runStep step = runIdentity . runStepT step



-- Util
iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
iterateUntilM p f v
  | p v       = return ()
  | otherwise = f v >>= iterateUntilM p f

-- Run
ended :: a -> Bool
ended = undefined

runGame :: Monad m => StepT m a -> Env -> m ()
runGame step = iterateUntilM ended exec
  where exec = runStepT (step >> env)

