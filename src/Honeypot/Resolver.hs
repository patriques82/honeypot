{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Resolver
  ( resolve
  ) where

import qualified Control.Monad.State as ST
import           Honeypot.Extract    (cell, runExt)
import           Honeypot.Prelude
import           Honeypot.Types
import           Lens.Simple         ((&), (.~), (^.))

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
  let es' = shift <$> (env ^. enemies)
  put (env & enemies .~ es')

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
  case runExt cell (env ^. player . pos) env of
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
  env@(Env board es player) <- get
  let view = playerView player (_dim board)
      pred e = any (== (current e)) view
  put (env & enemies .~ filter pred es)

adjustFuel :: (Int -> Int) -> EventCalc ()
adjustFuel f = do
  env <- get
  let fuel' = f (env ^. player . fuel)
  if fuel' < 0
     then lift Nothing -- end game
     else put (env & player . fuel .~ fuel')

turn :: (Dir -> Dir) -> EventCalc ()
turn f = do
  env <- get
  let dir' = f (env ^. player . dir)
  put (env & player . dir .~ dir')

move :: (Dir -> Pos -> Pos) -> EventCalc ()
move f = do
  env <- get
  let pos' = f (env ^. player . dir) (env ^. player . pos)
  if not (outOfBounds (env ^. board . dim) pos')
    then put (env & player . pos .~ pos')
    else return ()

