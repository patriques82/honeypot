{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Control.Arrow        (first, second)
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Data.Monoid          (Sum (..))
import           Data.Semigroup
import           Prelude              (Applicative, Bool (..), Bounded, Enum,
                                       Eq, IO, Int, Maybe (..), Ord, Show, not,
                                       subtract, undefined, ($), (+), (-), (.),
                                       (<), (<$>), (>), (>=), (||))

someFunc :: IO ()
someFunc = undefined

-- Rules
-- Move, ChangeDir = 1 fuel
-- Shoot = 5 fuel
-- Collide = 10 fuel
-- EnemyProximity = 10 fuel


-- Data Types
type Pos = (Int, Int)

type Dim = Pos

type Fuel = Int

data Dir = Left | Up | Right | Down
  deriving (Eq, Ord, Enum, Bounded)

data Event = TurnLeft     -- 1 fuel
           | TurnRight    -- 1 fuel
           | MoveForward  -- 1 fuel
           | Shoot        -- 5 fuel
  deriving (Eq, Ord)

data Player = Player { fuel :: Fuel
                     , dir  :: Dir
                     , pos  :: Pos
                     }

data Cell = Empty | Wall | Block | Enemy
  deriving Show

instance Semigroup Cell where
  Empty <> x = x
  y     <> _ = y

data Env = Env { dim    :: Dim
               , cells  :: M.Map Pos Cell
               , player :: Player
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
dim' = dim <$> env'

player' :: Step Player
player' = player <$> env'


-- Exported
-- Identify obstacle in front
identify :: Step Cell
identify = undefined

lidarBack :: Step Int
lidarBack = undefined

lidarFront :: Step Int
lidarFront = undefined

lidarLeft :: Step Int
lidarLeft = undefined

lidarRight :: Step Int
lidarRight = undefined



-- DSL for finding cells in map
type Extract a = Dim -> Pos -> M.Map Pos Cell -> a

-- primitives
cell :: Extract Cell
cell d p m = if not (outOfBounds d p)
                then case M.lookup p m of
                  Just o  -> o
                  Nothing -> Empty
                else Wall

-- helper
outOfBounds :: Dim -> Pos -> Bool
outOfBounds (xx,yy) (x,y) =
  x > xx || x < 0 || y > yy || y < 0



-- combinators
up :: Extract a -> Extract a
up ex d p m = ex d (second (subtract 1) p) m -- Arrow second

eMap :: (a -> b) -> Extract a -> Extract b
eMap f ext d p m = f $ ext d p m

until :: Semigroup a => Extract a -> (Extract a -> Extract a) -> (Cell -> Bool) -> Extract a
until ext trans pred d p m =
  let c = cell d p m
      found = pred c
  in if found
        then ext d p m
        else ext d p m <> trans (until ext trans pred) d p m

-- helpers
numerate :: Cell -> Sum Int
numerate Empty = Sum 1
numerate _     = Sum 0

notEmpty :: Cell -> Bool
notEmpty Empty = False
notEmpty _     = True


-- (until cell up notEmpty) d p m = Enemy
-- (until (eMap numerate cell) up notEmpty = Sum 3




-- provided by user
playerStep :: Step Event
playerStep = undefined


-- exported
frames :: Step Event -> Env -> [Env]
frames step env = let event = runStep step env
                  in case resolve env event of
                    Just env' -> env' : frames step env'
                    Nothing   -> []


-- TODO parts of resolve subsystem
resolve ::  Env -> Event -> Maybe Env
resolve = undefined
