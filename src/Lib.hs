{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Lib where

import Prelude as P
import Data.Foldable (foldl')
import Data.Either (isLeft)
import Data.List (sort)
import qualified Data.Map as M
import Control.Lens
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader


someFunc :: IO ()
someFunc = putStrLn "someFunc"



-- Data
data Pos = Pos { _x :: Int
               , _y :: Int
               } deriving (Eq, Ord)

data Dir = Left | Up | Right | Down
  deriving (Eq, Ord, Enum, Bounded)

data Variant = Spinner Dir 
             | Walker Pos Pos
  deriving Eq

data Enemy = Enemy { _eLife :: Integer
                   , _eDir :: Dir
                   , _variant :: Variant
                   } deriving Eq

data Player = Player { _pLife :: Integer
                     , _pDir :: Dir
                     }

data Environment = Env { _obstacles :: [Pos]
                       , _enemies :: M.Map Pos Enemy
                       , _player :: (Pos, Player)
                       , _dim :: Pos
                       }

data Event = ChangeDir Pos Dir
           | Move Pos Dir
           | Shoot Pos
  deriving (Eq, Ord)

makeLenses ''Pos
makeLenses ''Enemy
makeLenses ''Environment


-- Combinators

-- Similar to a State Monad with the difference that the state change is withheld
-- until the total step is runned so that intermediate steps doesnt know the
-- state changes that are occuring during the execution of the total step.
newtype StepT m a = StepT (ReaderT Environment m a)
  deriving (Functor, Applicative, Monad, MonadReader Environment, MonadIO)

type Step a = StepT Identity a

environment :: Monad m => StepT m Environment
environment = StepT ask

runStepT :: Monad m => StepT m a -> Environment -> m a
runStepT (StepT step) env = runReaderT step env

runStep :: Step a -> Environment -> a
runStep step = runIdentity . runStepT step



-- Util
iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
iterateUntilM p f v
  | p v       = return ()
  | otherwise = f v >>= iterateUntilM p f



-- Run
ended :: a -> Bool
ended e = undefined

runGame :: Monad m => StepT m a -> Environment -> m ()
runGame step = iterateUntilM ended exec
  where exec e = runStepT (step >> environment) e

