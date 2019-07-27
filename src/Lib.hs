{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Control.Monad.Reader
import qualified Data.Map             as M
import           Data.Monoid          (Sum (..))
import           Data.Semigroup
import           Prelude              (Applicative, Bool (..), Bounded, Enum,
                                       Eq, IO, Int, Maybe (..), Monad, Ord,
                                       Show, not, pure, return, undefined, ($),
                                       (+), (-), (.), (<), (<$>), (<*>), (>),
                                       (>=), (>>=), (||))

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


data Cell = Empty | Wall | Block | Enemy
  deriving Show

instance Semigroup Cell where
  Empty <> x = x
  y     <> _ = y

data Env = Env { dim   :: Dim
               , cells :: M.Map Pos Cell
               , dir   :: Dir
               , pos   :: Pos
               , fuel  :: Fuel
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


-- Exported
-- Identify obstacle in front
identify :: Step Cell
identify = do
  Env dim cells dir pos _ <- env'
  let extract = case dir of
                  Left  -> until notEmpty cell left
                  Right -> until notEmpty cell right
                  Down  -> until notEmpty cell down
                  Up    -> until notEmpty cell up
  return (runExt extract dim pos cells)

lidarBack :: Step Int
lidarBack = do
  Env dim cells dir pos _ <- env'
  let extract = case dir of
                  Left  -> until notEmpty (enumerate <$> cell) right
                  Right -> until notEmpty (enumerate <$> cell) left
                  Down  -> until notEmpty (enumerate <$> cell) up
                  Up    -> until notEmpty (enumerate <$> cell) down
  return $ getSum (runExt extract dim pos cells)

lidarFront :: Step Int
lidarFront = do
  Env dim cells dir pos _ <- env'
  let extract = case dir of
                  Left  -> until notEmpty (enumerate <$> cell) left
                  Right -> until notEmpty (enumerate <$> cell) right
                  Down  -> until notEmpty (enumerate <$> cell) down
                  Up    -> until notEmpty (enumerate <$> cell) up
  return $ getSum (runExt extract dim pos cells)

lidarLeft :: Step Int
lidarLeft = do
  Env dim cells dir pos _ <- env'
  let extract = case dir of
                  Left  -> until notEmpty (enumerate <$> cell) down
                  Right -> until notEmpty (enumerate <$> cell) up
                  Down  -> until notEmpty (enumerate <$> cell) right
                  Up    -> until notEmpty (enumerate <$> cell) left
  return $ getSum (runExt extract dim pos cells)

lidarRight :: Step Int
lidarRight = do
  Env dim cells dir pos _ <- env'
  let extract = case dir of
                  Left  -> until notEmpty (enumerate <$> cell) up
                  Right -> until notEmpty (enumerate <$> cell) down
                  Down  -> until notEmpty (enumerate <$> cell) left
                  Up    -> until notEmpty (enumerate <$> cell) right
  return $ getSum (runExt extract dim pos cells)


-- DSL for finding cells in map
newtype Extract a = Ext { runExt :: Dim -> Pos -> M.Map Pos Cell -> a }

instance Functor Extract where
  fmap f (Ext g) = Ext $ \d p m -> f (g d p m)

instance Applicative Extract where
  pure x = Ext $ \d p m -> x
  (Ext f) <*> (Ext x) = Ext $ \d p m -> (f d p m) (x d p m)

instance Monad Extract where
  return = pure
  (Ext x) >>= f = Ext $ \d p m ->
    runExt (f (x d p m)) d p m

-- primitives
cell :: Extract Cell
cell = Ext $ \d p m ->
  if not (outOfBounds d p)
     then case M.lookup p m of
       Just o  -> o
       Nothing -> Empty
     else Wall

outOfBounds :: Dim -> Pos -> Bool
outOfBounds (xx,yy) (x,y) =
  x > xx || x < 0 || y > yy || y < 0



-- combinators
up :: Extract a -> Extract a
up (Ext f) = Ext $ \d (x,y) m ->
  f d (x,y-1) m

down :: Extract a -> Extract a
down (Ext f) = Ext $ \d (x,y) m ->
  f d (x,y+1) m

left :: Extract a -> Extract a
left (Ext f) = Ext $ \d (x,y) m ->
  f d (x-1,y) m

right :: Extract a -> Extract a
right (Ext f) = Ext $ \d (x,y) m ->
  f d (x+1,y) m

until :: Semigroup a => (Cell -> Bool) -> Extract a -> (Extract a -> Extract a) -> Extract a
until pred ext trans = do
  c <- cell
  if pred c
     then ext
     else (<>) <$> ext <*> trans (until pred ext trans)

-- helpers
enumerate :: Cell -> Sum Int
enumerate Empty = Sum 1
enumerate _     = Sum 0

notEmpty :: Cell -> Bool
notEmpty Empty = False
notEmpty _     = True


-- provided by user
playerStep :: Step Event
playerStep = undefined


-- exported
frames :: Step Event -> Env -> [Env]
frames step env =
  let event = runStep step env
  in case resolve env event of
    Just env' -> env' : frames step env'
    Nothing   -> []


-- TODO parts of resolve subsystem
resolve ::  Env -> Event -> Maybe Env
resolve = undefined
