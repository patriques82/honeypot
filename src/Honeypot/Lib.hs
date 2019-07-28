{-# LANGUAGE NoImplicitPrelude #-}

module Honeypot.Lib where

import           Data.Monoid      (Sum (..))
import           Data.Semigroup
import           Honeypot.Extract
import           Honeypot.Prelude
import           Honeypot.Types

-- Rules
-- Move, ChangeDir = 1 fuel
-- Shoot = 5 fuel
-- Collide = 10 fuel
-- EnemyProximity = 10 fuel




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
                  Left  -> until notEmpty (countEmpty <$> cell) right
                  Right -> until notEmpty (countEmpty <$> cell) left
                  Down  -> until notEmpty (countEmpty <$> cell) up
                  Up    -> until notEmpty (countEmpty <$> cell) down
  return $ getSum (runExt extract dim pos cells)

lidarFront :: Step Int
lidarFront = do
  Env dim cells dir pos _ <- env'
  let extract = case dir of
                  Left  -> until notEmpty (countEmpty <$> cell) left
                  Right -> until notEmpty (countEmpty <$> cell) right
                  Down  -> until notEmpty (countEmpty <$> cell) down
                  Up    -> until notEmpty (countEmpty <$> cell) up
  return $ getSum (runExt extract dim pos cells)

lidarLeft :: Step Int
lidarLeft = do
  Env dim cells dir pos _ <- env'
  let extract = case dir of
                  Left  -> until notEmpty (countEmpty <$> cell) down
                  Right -> until notEmpty (countEmpty <$> cell) up
                  Down  -> until notEmpty (countEmpty <$> cell) right
                  Up    -> until notEmpty (countEmpty <$> cell) left
  return $ getSum (runExt extract dim pos cells)

lidarRight :: Step Int
lidarRight = do
  Env dim cells dir pos _ <- env'
  let extract = case dir of
                  Left  -> until notEmpty (countEmpty <$> cell) up
                  Right -> until notEmpty (countEmpty <$> cell) down
                  Down  -> until notEmpty (countEmpty <$> cell) left
                  Up    -> until notEmpty (countEmpty <$> cell) right
  return $ getSum (runExt extract dim pos cells)


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
