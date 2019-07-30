{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Resolver
  ( resolve
  ) where

import           Control.Monad.State
import           Honeypot.Prelude
import           Honeypot.Types

newtype EventCalc a = EventCalc { runCalc :: StateT Env Maybe a }
  deriving (Functor, Applicative, Monad, MonadState Env)

resolve :: Env -> Map Pos Event -> Event -> Maybe Env
resolve env enemyEvents playerEvent =
  execStateT (runCalc (events enemyEvents playerEvent)) env

events :: Map Pos Event -> Event -> EventCalc ()
events enemyEvents playerEvent = do
  execEnemies enemyEvents
  execPlayer playerEvent


-- Enemy events
execEnemies :: Map Pos Event -> EventCalc ()
execEnemies events = do
  calculateMovements events
  calculateTurns events

calculateMovements :: Map Pos Event -> EventCalc ()
calculateMovements events = do
  env <- get
  put env { enemies = foldrWithKey' moveForward' (enemies env) events }

moveForward' :: Pos -> Event -> Matrix (Maybe Enemy) -> Matrix (Maybe Enemy)
moveForward' pos@(x,y) MoveForward m =
  case m ! pos of
    Nothing    ->
      m
    Just enemy ->
      let m' = setElem Nothing pos m
          pos' = forward (eDir enemy) pos
      in setElem (Just enemy) pos' m'
moveForward' _ _ m = m

calculateTurns :: Map Pos Event -> EventCalc ()
calculateTurns events = do
  env <- get
  put env { enemies = foldrWithKey' turn' (enemies env) events }

turn' :: Pos -> Event -> Matrix (Maybe Enemy) -> Matrix (Maybe Enemy)
turn' pos TurnRight m =
  case m ! pos of
    Nothing    ->
      m
    Just enemy ->
      let enemy' = enemy { eDir = right (eDir enemy) }
      in setElem (Just enemy') pos m
turn' pos TurnLeft m  =
  case m ! pos of
    Nothing    ->
      m
    Just enemy ->
      let enemy' = enemy { eDir = left (eDir enemy) }
      in setElem (Just enemy') pos m
turn' _ _ m           = m


-- Player event
execPlayer :: Event -> EventCalc ()
execPlayer e = do
  case e of
    TurnLeft    -> turnLeft    -- 1 fuel
    TurnRight   -> turnRight   -- 1 fuel
    MoveForward -> moveForward -- 1 fuel
    Shoot       -> shoot       -- 5 fuel
  calculateCollisions
  calculateHits

-- may return Nothing (player killed) and shortcurcuit the rest of the calculations
calculateCollisions :: EventCalc ()
calculateCollisions = undefined

-- how much fuel?
calculateHits :: EventCalc ()
calculateHits = undefined

turnLeft, turnRight, moveForward, shoot :: EventCalc ()
turnLeft = undefined

turnRight = undefined

moveForward = undefined

-- kill all enemies
shoot = undefined

