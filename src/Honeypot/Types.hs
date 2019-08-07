{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Types where

import           Control.Monad.Reader (MonadReader, Reader, ask, runReader)
import           Data.Matrix
import           Honeypot.Prelude

type Pos = (Int, Int) -- row, col, index starts at 1,1

type Dim = Pos

type Fuel = Int

data Dir = West
         | North
         | East
         | South

forward :: Dir -> Pos -> Pos
forward West (y,x)  = (y,x-1)
forward East (y,x)  = (y,x+1)
forward North (y,x) = (y-1,x)
forward South (y,x) = (y+1,x)

backward :: Dir -> Pos -> Pos
backward West (y,x)  = (y,x+1)
backward East (y,x)  = (y,x-1)
backward North (y,x) = (y+1,x)
backward South (y,x) = (y-1,x)

right :: Dir -> Dir
right West  = North
right East  = South
right North = East
right South = West

left :: Dir -> Dir
left West  = South
left East  = North
left North = West
left South = East

outOfBounds :: Dim -> Pos -> Bool
outOfBounds (yy,xx) (y,x) =
  x > xx || x < 1 || y > yy || y < 1

data Event = TurnLeft     -- 1 fuel
           | TurnRight    -- 1 fuel
           | MoveForward  -- 1 fuel
           | MoveBackward -- 1 fuel
           | Shoot        -- 5 fuel

data Enemy = E { future  :: [Pos]
               , current :: !Pos
               , past    :: [Pos]
               }

shift :: Enemy -> Enemy
shift e@(E [] p [])   = e
shift (E [] p (y:ys)) = E ys y [p]
shift (E (x:xs) p ys) = E xs x (p:ys)

data Cell = Empty
          | Wall
          | Block
          | Enemy

instance Semigroup Cell where
  Empty <> x = x
  y     <> _ = y

data Board = Board { dim     :: Dim
                   , terrain :: Matrix Bool
                   }

data Env = Env { board   :: Board
               , enemies :: [Enemy]
               , dir     :: Dir
               , pos     :: Pos
               , fuel    :: Fuel
               }

playerView :: Dir -> Pos -> Dim -> [Pos]
playerView dir (y,x) dim =
  case dir of
    West  -> f (y,x-1)
    North -> f (y-1,x)
    East  -> f (y,x+1)
    South -> f (y+1,x)
  where f p = if not (outOfBounds dim p)
                 then p:playerView dir p dim
                 else []

newtype Step a = Step (Reader Env a) -- hide constructor on export
  deriving (Functor, Applicative, Monad, MonadReader Env)

runStep :: Step a -> Env -> a
runStep (Step step) = runReader step

env :: Step Env
env = Step ask
