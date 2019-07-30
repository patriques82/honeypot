{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Resolver
  ( resolve
  ) where

import           Control.Monad.State
import           Honeypot.Extract
import           Honeypot.Prelude
import           Honeypot.Types

--data Event = TurnLeft     -- 1 fuel
           -- | TurnRight    -- 1 fuel
           -- | MoveForward  -- 1 fuel
           -- | Shoot        -- 5 fuel

--data Env = Env { dim   :: Dim
               --, cells :: Map Pos Cell
               --, dir   :: Dir
               --, pos   :: Pos
               --, fuel  :: Fuel
               --}

newtype EventCalc a = EventCalc { runCalc :: StateT Env Maybe a }
  deriving (Functor, Applicative, Monad, MonadState Env)

resolve :: Env -> Map Pos Event -> Event -> Maybe Env
resolve env enemyEvents playerEvent =
  execStateT (runCalc (events enemyEvents playerEvent)) env

events :: Map Pos Event -> Event -> EventCalc ()
events enemyEvents playerEvent = do
  execEnemies enemyEvents
  execPlayer playerEvent

execEnemies :: Map Pos Event -> EventCalc ()
execEnemies events = do
  calculateMovements events
  calculateTurns events

execPlayer :: Event -> EventCalc ()
execPlayer e = do
  case e of
    TurnLeft    -> turnLeft -- 1 fuel
    TurnRight   -> turnRight -- 1 fuel
    MoveForward -> moveForward -- 1 fuel
    Shoot       -> shoot -- 5 fuel
  calculateCollisions
  calculateHits

calculateMovements :: Map Pos Event -> EventCalc ()
calculateMovements events =
  let moves = flip filter events $ \case
                                      MoveForward -> True
                                      _           -> False
  in do
    env <- get
    put env

calculateTurns :: Map Pos Event -> EventCalc ()
calculateTurns = undefined

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

