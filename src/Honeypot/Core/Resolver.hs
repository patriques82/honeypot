{-# LANGUAGE BangPatterns #-}

module Honeypot.Core.Resolver
  ( resolve
  ) where

import           Honeypot.Core.EventCalc
import           Honeypot.Core.Extract   (cell, runExt)
import           Honeypot.Prelude
import           Honeypot.Types
import           Lens.Simple             ((%~), (&), (.~), (^.))

resolve :: Env -> Event -> Either Status Env
resolve env event = execEventCalc (exec event) env

exec :: Event -> EventCalc ()
exec e = do
  execPlayer e
  execEnemies

execEnemies :: EventCalc ()
execEnemies = do
  env <- get
  let !es' = step <$> (env ^. enemies)
  put (env & enemies .~ es')

-- Player event
execPlayer :: Event -> EventCalc ()
execPlayer e = do
  case e of
    TurnLeft     -> turnLeft     -- 1 fuel
    TurnRight    -> turnRight    -- 1 fuel
    MoveForward  -> moveForward  -- 1 fuel
    MoveBackward -> moveBackward -- 1 fuel
    Noop         -> noop         -- 1 fuel
    Shoot        -> shoot        -- 5 fuel
  calculateCollisions            -- 10 fuel

-- may return Nothing (player killed) and shortcurcuit the rest of the calculations
calculateCollisions :: EventCalc ()
calculateCollisions = do
  env <- get
  case runExt cell (env ^. player . pos) env of
    Empty -> return ()
    _     -> adjustFuel (subtract 10) -- wall, block or enemy

turnLeft, turnRight, moveForward, moveBackward, noop, shoot :: EventCalc ()
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

noop = adjustFuel (subtract 1)

shoot = do
  adjustFuel (subtract 5)
  removeEnemies

removeEnemies :: EventCalc ()
removeEnemies = do
  env@(Env terrain enemies' player) <- get
  let !view = playerView (unBoard terrain) player
      pred e = current e `elem` view
  put (env & enemies .~ filter pred enemies')

adjustFuel :: (Int -> Int) -> EventCalc ()
adjustFuel f = do
  env <- get
  let !fuel' = f (env ^. player . fuel)
  if fuel' < 0
     then lift (Left Lost)
     else put (env & player . fuel .~ fuel')

turn :: (Dir -> Dir) -> EventCalc ()
turn f = modify (\env -> env & player . dir %~ f)

move :: (Dir -> Pos -> Pos) -> EventCalc ()
move f = do
  env <- get
  let !pos' = f (env ^. player . dir) (env ^. player . pos)
  case unBoard (env ^. terrain) !? pos' of
    Nothing -> return ()
    _       -> put (env & player . pos .~ pos')
