{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Control.Monad.Reader
import qualified Data.Map             as M

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Rules
-- Move, ChangeDir = 1 fuel
-- Shoot = 5 fuel
-- Collide = 10 fuel
-- EnemyProximity = 10 fuel


-- Data Types
data Pos = Pos Int Int
  deriving (Eq, Ord)

type Dim = Pos

type Fuel = Int

data Dir = Left | Up | Right | Down
  deriving (Eq, Ord, Enum, Bounded)

data Event = Left -- 1 fuel
           | Right -- 1 fuel
           | Move -- 1 fuel
           | Shoot -- 5 fuel
  deriving (Eq, Ord)

newtype Player = Player { _fuel :: Fuel
                        , _dir  :: Dir 
                        }

data Obstacle = Wall | Block | Enemy

data Env = Env { _dim       :: Dim
               , _obstacles :: Pos -> Obstacle -- M.Map Pos Obstacle
               , _player    :: Player
               }



-- Step Combinators

-- Similar to a State Monad with the difference that the state change is withheld
-- until the total step is runned so that intermediate steps doesnt know the
-- state changes that are occuring during the execution of the total step.

newtype Step a = Step (Reader Env a) -- hide constructor on export
  deriving (Functor, Applicative, Monad, MonadReader Env)


runStep :: Step a -> Env -> a
runStep (Step step) = runReader step


-- Internal
env' :: Step Env
env' = Step ask

dim' :: Step Dim
dim' = _dim <$> env'

obstacles' :: Step (M.Map Pos Obstacle)
obstacles' = _obstacles <$> env'

player' :: Step (Pos, Player)
player' = _player <$> env'


-- Exported
-- Identify obstacle infront
identify :: Step Obstacle
identify = undefined

lidarBack :: Step Int
lidarBack = undefined

lidarFront :: Step Int
lidarFront = undefined

lidarLeft :: Step Int
lidarLeft = undefined

lidarRight :: Step Int
lidarRight = undefined





-- provided by user
playerStep :: Step Event
playerStep = undefined


-- exported
frames :: Step Event -> Env -> [Env]
frames step env = let e = runStep step env
                  in case resolve env e of
                    Just env' -> e : frames step env'
                    Nothing   -> []


-- TODO parts of resolve subsystem
resolve ::  Env -> Event -> Maybe Env
resolve = undefined
