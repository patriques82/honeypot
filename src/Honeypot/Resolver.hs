{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Resolver
  ( resolve
  ) where

import qualified Control.Monad.State as ST
import           Honeypot.Prelude
import           Honeypot.Types

newtype EventCalc a = EventCalc { runCalc :: ST.StateT Env Maybe a }
  deriving (Functor, Applicative, Monad, ST.MonadState Env)

lift :: Maybe a -> EventCalc a
lift m = EventCalc $ ST.lift m

put :: Env -> EventCalc ()
put = ST.put

get :: EventCalc Env
get = ST.get

resolve :: Env -> Map Pos Event -> Event -> Maybe Env
resolve env enemyEvents playerEvent =
  ST.execStateT (runCalc (execEvents enemyEvents playerEvent)) env

execEvents :: Map Pos Event -> Event -> EventCalc ()
execEvents enemyEvents playerEvent = do
  execEnemies enemyEvents
  execPlayer playerEvent

-- Enemy events
execEnemies :: Map Pos Event -> EventCalc ()
execEnemies events = do
  env <- get
  put env { enemies = foldrWithKey' move' (enemies env) events }
  where
    move' pos@(x,y) ev m =
      case (m ! pos, ev) of
        (Nothing, _) -> m
        (Just enemy, MoveForward) ->
          let m' = setElem Nothing pos m
              pos' = forward (eDir enemy) pos
          in setElem (Just enemy) pos' m'
        (Just enemy, MoveBackward) ->
          let m' = setElem Nothing pos m
              pos' = backward (eDir enemy) pos
          in setElem (Just enemy) pos' m'
        (Just enemy, TurnRight) ->
          let enemy' = enemy { eDir = right (eDir enemy) }
          in setElem (Just enemy') pos m
        (Just enemy, TurnLeft) ->
          let enemy' = enemy { eDir = left (eDir enemy) }
          in setElem (Just enemy') pos m

-- Player event
execPlayer :: Event -> EventCalc ()
execPlayer e = do
  case e of
    TurnLeft     -> turnLeft     -- 1 fuel
    TurnRight    -> turnRight    -- 1 fuel
    MoveForward  -> moveForward  -- 1 fuel
    MoveBackward -> moveBackward -- 1 fuel
    Shoot        -> shoot        -- 5 fuel
  calculateCollisions            -- 10 fuel

-- may return Nothing (player killed) and shortcurcuit the rest of the calculations
calculateCollisions :: EventCalc ()
calculateCollisions = undefined

turnLeft, turnRight, moveForward, moveBackward, shoot :: EventCalc ()

turnLeft = do
  adjustFuel (subtract 1)
  turn left

turnRight = do
  adjustFuel (subtract 1)
  turn right

moveForward = do
  adjustFuel (subtract 1)
  move forward

moveBackward = do
  adjustFuel (subtract 1)
  move backward

shoot = do
  adjustFuel (subtract 5)
  removeEnemies

removeEnemies :: EventCalc ()
removeEnemies = do
  env <- get
  let enemies' = go (outOfBounds (dim env)) (forward (pDir env)) (pPos env) (enemies env)
  put env { enemies = enemies' }
  where
    go :: (Pos -> Bool) -> (Pos -> Pos) -> Pos -> Matrix (Maybe Enemy) -> Matrix (Maybe Enemy)
    go pred shift p m =
      if pred p
         then m
         else go pred shift (shift p) (setElem Nothing p m)

adjustFuel :: (Int -> Int) -> EventCalc ()
adjustFuel f = do
  env <- get
  let fuel' = f (pFuel env)
  if fuel' < 0
     then lift Nothing -- end game
     else put env { pFuel = fuel' }

turn :: (Dir -> Dir) -> EventCalc ()
turn f = do
  env <- get
  let dir' = f (pDir env)
  put env { pDir = dir' }

move :: (Dir -> Pos -> Pos) -> EventCalc ()
move f = do
  env <- get
  let pos' = f (pDir env) (pPos env)
  put env { pPos = pos' }
