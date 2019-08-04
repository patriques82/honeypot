{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Resolver
  ( resolve
  ) where

import qualified Control.Monad.State as ST
import           Honeypot.Extract    (cell, runExt)
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

resolve :: Env -> Event -> Maybe Env
resolve env event =
  ST.execStateT (runCalc (exec event)) env

exec :: Event -> EventCalc ()
exec e = do
  execPlayer e
  execEnemies

execEnemies :: EventCalc ()
execEnemies = do
  env <- get
  let shift (E pos dir ev) =
        case ev dir pos of
          Start d -> undefined
          Move d  -> E (forward d pos) dir ev
          End d   -> E pos (toggle dir) ev
      es' = map shift (enemies env)
  put env { enemies = es' }

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
calculateCollisions = do
  env <- get
  case runExt cell (pos env) env of
    Empty -> return ()
    _     -> adjustFuel (subtract 10) -- wall, block or enemy

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
  env@(Env board es dir pos _) <- get
  let view = fromList $ playerView dir pos (dim board)
      pred e = member (pathPos e) view
  put env { enemies = filter pred es }

adjustFuel :: (Int -> Int) -> EventCalc ()
adjustFuel f = do
  env <- get
  let fuel' = f (fuel env)
  if fuel' < 0
     then lift Nothing -- end game
     else put env { fuel = fuel' }

turn :: (Dir -> Dir) -> EventCalc ()
turn f = do
  env <- get
  let dir' = f (dir env)
  put env { dir = dir' }

move :: (Dir -> Pos -> Pos) -> EventCalc ()
move f = do
  env <- get
  let pos' = f (dir env) (pos env)
  if not (outOfBounds (dim (board env)) pos')
    then put env { pos = pos' }
    else return ()
