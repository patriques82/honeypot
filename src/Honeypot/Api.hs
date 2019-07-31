{-# LANGUAGE NoImplicitPrelude #-}

-- This module should be the only public
module Honeypot.Api
  ( identifyTarget
  , lidarBack
  , lidarFront
  , lidarLeft
  , lidarRight
  , frames
  -- reexported types
  , Step
  , Event (..)
  , Cell (..)
  , Dir (..)
  , Pos
  , Dim
  , Fuel
  ) where

import           Data.Monoid       (Sum (..))
import           Data.Semigroup
import           Honeypot.Extract
import           Honeypot.Prelude
import           Honeypot.Resolver
import           Honeypot.Types

currentFuel :: Step Int
currentFuel = pFuel <$> env

-- Identify obstacle in front
identifyTarget :: Step Cell
identifyTarget = do
  e <- env
  let extract = case pDir e of
                  Left  -> until notEmpty cell leftE
                  Right -> until notEmpty cell rightE
                  Down  -> until notEmpty cell downE
                  Up    -> until notEmpty cell upE
  return (runExt extract (pPos e) e)

lidarBack :: Step Int
lidarBack = do
  e <- env
  let extract = case pDir e of
                  Left  -> until notEmpty (countEmpty <$> cell) rightE
                  Right -> until notEmpty (countEmpty <$> cell) leftE
                  Down  -> until notEmpty (countEmpty <$> cell) upE
                  Up    -> until notEmpty (countEmpty <$> cell) downE
  return $ getSum (runExt extract (pPos e) e)

lidarFront :: Step Int
lidarFront = do
  e <- env
  let extract = case pDir e of
                  Left  -> until notEmpty (countEmpty <$> cell) leftE
                  Right -> until notEmpty (countEmpty <$> cell) rightE
                  Down  -> until notEmpty (countEmpty <$> cell) downE
                  Up    -> until notEmpty (countEmpty <$> cell) upE
  return $ getSum (runExt extract (pPos e) e)

lidarLeft :: Step Int
lidarLeft = do
  e <- env
  let extract = case pDir e of
                  Left  -> until notEmpty (countEmpty <$> cell) downE
                  Right -> until notEmpty (countEmpty <$> cell) upE
                  Down  -> until notEmpty (countEmpty <$> cell) rightE
                  Up    -> until notEmpty (countEmpty <$> cell) leftE
  return $ getSum (runExt extract (pPos e) e)

lidarRight :: Step Int
lidarRight = do
  e <- env
  let extract = case pDir e of
                  Left  -> until notEmpty (countEmpty <$> cell) upE
                  Right -> until notEmpty (countEmpty <$> cell) downE
                  Down  -> until notEmpty (countEmpty <$> cell) leftE
                  Up    -> until notEmpty (countEmpty <$> cell) rightE
  return $ getSum (runExt extract (pPos e) e)

-- provided by user
playerStep :: Step Event
playerStep = undefined

enemiesStep :: Step (Map Pos Event)
enemiesStep = undefined

frames :: Step (Map Pos Event) -> Step Event -> Env -> [Env]
frames enemies player env =
  let playerEv = player `runStep` env
      enemiesEv = enemies `runStep` env
  in case resolve env enemiesEv playerEv of
    Just env' -> env' : frames enemies player env'
    Nothing   -> []
