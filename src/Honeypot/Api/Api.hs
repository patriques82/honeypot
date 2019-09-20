{-# LANGUAGE NoImplicitPrelude #-}

module Honeypot.Api.Api
  ( exec
  , identifyTarget
  , lidarBack
  , lidarFront
  , lidarLeft
  , lidarRight
  , frames
  ) where

import           Honeypot.Api.Extract
import           Honeypot.Api.Resolver
import           Honeypot.Prelude
import           Honeypot.Types
import           Lens.Simple           ((&), (.~), (^.))

exec :: GameState -> Step Event -> GameState
exec st@(GameOver _) _   = st
exec (Continue env) step =
  case env |~ step of
    Left status -> GameOver status
    Right env'  -> Continue env'

(|~) :: Env -> Step Event -> Either Status Env
env |~ step = resolve env (step `runStep` env)

currentFuel :: Step Fuel
currentFuel = withEnv (\e -> e ^. player . fuel)

-- Identify obstacle in front
identifyTarget :: Step Cell
identifyTarget = do
  e <- env
  let extract = case e ^. player . dir of
                  West  -> until notEmpty cell leftE
                  East  -> until notEmpty cell rightE
                  South -> until notEmpty cell downE
                  North -> until notEmpty cell upE
  return (runExt extract (e ^. player . pos) e)

lidarBack :: Step Int
lidarBack = do
  e <- env
  let extract = case e ^. player . dir of
                  West  -> until notEmpty (countEmpty <$> cell) rightE
                  East  -> until notEmpty (countEmpty <$> cell) leftE
                  South -> until notEmpty (countEmpty <$> cell) upE
                  North -> until notEmpty (countEmpty <$> cell) downE
  return $ getSum (runExt extract (e ^. player . pos) e)

lidarFront :: Step Int
lidarFront = do
  e <- env
  let extract = case e ^. player . dir of
                  West  -> until notEmpty (countEmpty <$> cell) leftE
                  East  -> until notEmpty (countEmpty <$> cell) rightE
                  South -> until notEmpty (countEmpty <$> cell) downE
                  North -> until notEmpty (countEmpty <$> cell) upE
  return $ getSum (runExt extract (e ^. player . pos) e)

lidarLeft :: Step Int
lidarLeft = do
  e <- env
  let extract = case e ^. player . dir of
                  West  -> until notEmpty (countEmpty <$> cell) downE
                  East  -> until notEmpty (countEmpty <$> cell) upE
                  South -> until notEmpty (countEmpty <$> cell) rightE
                  North -> until notEmpty (countEmpty <$> cell) leftE
  return $ getSum (runExt extract (e ^. player . pos) e)

lidarRight :: Step Int
lidarRight = do
  e <- env
  let extract = case e ^. player . dir of
                  West  -> until notEmpty (countEmpty <$> cell) upE
                  East  -> until notEmpty (countEmpty <$> cell) downE
                  South -> until notEmpty (countEmpty <$> cell) leftE
                  North -> until notEmpty (countEmpty <$> cell) rightE
  return $ getSum (runExt extract (e ^. player . pos) e)

frames :: Step Event -> Env -> [Env]
frames playerStep env =
  case env |~ playerStep of
    Right env' -> env' : frames playerStep env'
    Left _     -> []
