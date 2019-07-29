{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Types where

import           Control.Monad.Reader (MonadReader, Reader, ask, runReader)
import           Honeypot.Prelude

type Pos = (Int, Int)

type Dim = Pos

type Fuel = Int

type Id = Int

data Dir = Left
         | Up
         | Right
         | Down

data Event = TurnLeft     -- 1 fuel
           | TurnRight    -- 1 fuel
           | MoveForward  -- 1 fuel
           | Shoot        -- 5 fuel

type Behaviour = Pos -> Dir -> Event

data Cell = Empty
          | Wall
          | Block
          | Enemy Dir Behaviour

instance Semigroup Cell where
  Empty <> x = x
  y     <> _ = y

data Env = Env { dim   :: Dim
               , cells :: Map Pos Cell
               , dir   :: Dir
               , pos   :: Pos
               , fuel  :: Fuel
               }

newtype Step a = Step (Reader Env a) -- hide constructor on export
  deriving (Functor, Applicative, Monad, MonadReader Env)

runStep :: Step a -> Env -> a
runStep (Step step) = runReader step

env' :: Step Env
env' = Step ask

