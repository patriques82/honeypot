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

forward :: Dir -> Pos -> Pos
forward Left (x,y)  = (x-1,y)
forward Right (x,y) = (x+1,y)
forward Up (x,y)    = (x,y-1)
forward Down (x,y)  = (x,y+1)

right :: Dir -> Dir
right Left  = Up
right Right = Down
right Up    = Right
right Down  = Left

left :: Dir -> Dir
left Left  = Down
left Right = Up
left Up    = Left
left Down  = Right

outOfBounds :: Pos -> Dim -> Bool
outOfBounds (x,y) (xx,yy) =
  x > xx || x < 0 || y > yy || y < 0

data Event = TurnLeft     -- 1 fuel
           | TurnRight    -- 1 fuel
           | MoveForward  -- 1 fuel
           | Shoot        -- 5 fuel

type Behaviour = Pos -> Dir -> Event

data Enemy = E { eDir :: Dir
               , beh  :: Behaviour
               }

data Block = B

data Cell = Empty
          | Wall
          | Block
          | Enemy

instance Semigroup Cell where
  Empty <> x = x
  y     <> _ = y

data Env = Env { dim     :: Dim
               , enemies :: Matrix (Maybe Enemy)
               , blocks  :: Matrix (Maybe Block)
               , pDir    :: Dir
               , pos     :: Pos
               , fuel    :: Fuel
               }

newtype Step a = Step (Reader Env a) -- hide constructor on export
  deriving (Functor, Applicative, Monad, MonadReader Env)

runStep :: Step a -> Env -> a
runStep (Step step) = runReader step

env' :: Step Env
env' = Step ask

