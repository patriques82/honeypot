{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Honeypot.Types where

import           Control.Monad.Reader (MonadReader, Reader, ask, asks,
                                       runReader)
import           Data.Matrix          (Matrix, ncols, nrows, safeGet, (!))
import           Honeypot.Prelude
import           Lens.Simple          (makeLenses)

data Pos = P !Int !Int -- row, col, index starts at 1,1
  deriving Eq

instance Show Pos where
  show (P y x) = "(" ++ show y ++ "," ++ show x ++ ")"

position :: (Int, Int) -> Pos
position (x, y) = P y x

type Dim = Pos

type Fuel = Int

data Dir = West
         | North
         | East
         | South
         deriving (Eq, Show)

forward :: Dir -> Pos -> Pos
forward West (P y x)  = P y (x-1)
forward East (P y x)  = P y (x+1)
forward North (P y x) = P (y-1) x
forward South (P y x) = P (y+1) x

backward :: Dir -> Pos -> Pos
backward West (P y x)  = P y (x+1)
backward East (P y x)  = P y (x-1)
backward North (P y x) = P (y+1) x
backward South (P y x) = P (y-1) x

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

type Board = Matrix Bool

getOccupied :: Board -> [Pos]
getOccupied b = foldr (\(y,x) xs -> if occupied b (y,x) then P y x : xs else xs) [] ps
  where ps = [(y',x') | y' <- [1..(ncols b)] , x' <- [1..(nrows b)]]

occupied :: Board -> (Int, Int) -> Bool
occupied = (!)

data Event = TurnLeft     -- 1 fuel
           | TurnRight    -- 1 fuel
           | MoveForward  -- 1 fuel
           | MoveBackward -- 1 fuel
           | Noop         -- 1 fuel
           | Shoot        -- 5 fuel

data Enemy = E { future  :: ![Pos]
               , current :: !Pos
               , past    :: ![Pos]
               } deriving (Eq, Show)

step :: Enemy -> Enemy
step e@(E [] p [])   = e
step (E [] p (y:ys)) = E ys y [p]
step (E (x:xs) p ys) = E xs x (p:ys)

data Cell = Empty
          | Wall
          | Block
          | Enemy

instance Semigroup Cell where
  Empty <> x = x
  y     <> _ = y

data Player = Player { _dir  :: !Dir
                     , _pos  :: !Pos
                     , _fuel :: !Fuel
                     } deriving (Eq, Show)

playerView :: Matrix a -> Player -> [Pos]
playerView m (Player dir p _) = go (forward dir p)
  where go p = case m !? p of
                 Nothing -> []
                 Just _  -> p : go (forward dir p)

data Env = Env { _terrain :: !Board
               , _enemies :: ![Enemy]
               , _player  :: !Player
               }

data Status = Lost | Won

data GameState = GameOver Status
               | Continue Env

(!?) :: Matrix a -> Pos -> Maybe a
m !? (P y x) = safeGet y x m

$(makeLenses ''Player)
$(makeLenses ''Env)

newtype Step a = Step (Reader Env a) -- hide constructor on export
  deriving (Functor, Applicative, Monad, MonadReader Env)

runStep :: Step a -> Env -> a
runStep (Step step) = runReader step

env :: Step Env
env = Step ask

withEnv :: (Env -> a) -> Step a
withEnv = Step . asks
