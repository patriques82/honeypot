{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Resolver
  ( resolve
  ) where

import qualified Control.Monad.State as ST
import           Honeypot.Extract    (cell, runExt)
import           Honeypot.Prelude
import           Honeypot.Types
import           Lens.Simple         ((%~), (&), (.~), (^.))

newtype EventCalc a = EventCalc { runCalc :: ST.StateT Env Maybe a }
  deriving (Functor, Applicative, Monad, ST.MonadState Env)

lift :: Maybe a -> EventCalc a
lift m = EventCalc $ ST.lift m

put :: Env -> EventCalc ()
put = ST.put

get :: EventCalc Env
get = ST.get

modify :: (Env -> Env) -> EventCalc ()
modify = ST.modify

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
  let !view = playerView terrain player
      pred e = any (== (current e)) view
  put (env & enemies .~ filter pred enemies')

adjustFuel :: (Int -> Int) -> EventCalc ()
adjustFuel f = do
  env <- get
  let !fuel' = f (env ^. player . fuel)
  if fuel' < 0
     then lift Nothing -- end game >TODO (Either?)
     else put (env & player . fuel .~ fuel')

turn :: (Dir -> Dir) -> EventCalc ()
turn f = modify (\env -> env & player . dir %~ f)

move :: (Dir -> Pos -> Pos) -> EventCalc ()
move f = do
  env <- get
  let !pos' = f (env ^. player . dir) (env ^. player . pos)
  case (env ^. terrain) !? pos' of
    Nothing -> return ()
    _       -> put (env & player . pos .~ pos')
