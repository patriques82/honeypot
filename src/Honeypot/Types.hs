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


data PathDir = Forward | Backward
  deriving Eq

toggle :: PathDir -> PathDir
toggle Forward  = Backward
toggle Backward = Forward

data PathEvent = Start Dir
               | Move Dir
               | End Dir

data Enemy = E { pathPos   :: Pos
               , pathDir   :: PathDir
               , pathEvent :: PathDir -> Pos -> PathEvent
               }


instance Eq Enemy where
  (E p1 d1 _) == (E p2 d2 _) = p1 == p2 && d1 == d2

instance Ord Enemy where
  (E p1 _ _) <= (E p2 _ _) = p1 <= p2

data Block = B

data Cell = Empty
          | Wall
          | Block
          | Enemy

instance Semigroup Cell where
  Empty <> x = x
  y     <> _ = y

data Board = Board { dim     :: Dim
                   , terrain :: Matrix (Maybe Block)
                   }

data Env = Env { board   :: Board
               , enemies :: Set Enemy
               , dir     :: Dir
               , pos     :: Pos
               , fuel    :: Fuel
               }

playerView :: Dir -> Pos -> Dim -> [Pos]
playerView dir (y,x) dim =
  case dir of
    West  -> go (y,x-1)
    North -> go (y-1,x)
    East  -> go (y,x+1)
    South -> go (y+1,x)
  where go p = if not (outOfBounds dim p)
                  then p:playerView dir p dim
                  else []

newtype Step a = Step (Reader Env a) -- hide constructor on export
  deriving (Functor, Applicative, Monad, MonadReader Env)

runStep :: Step a -> Env -> a
runStep (Step step) = runReader step

env :: Step Env
env = Step ask
