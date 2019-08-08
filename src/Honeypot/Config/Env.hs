{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Config.Env
  ( Config (..)
  , ConfigEval
  , evalConfig
  ) where

import           Control.Monad.Except (Except, MonadError, throwError)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, runReader)
import           Data.Matrix          (fromList, matrix, setElem)
import           Honeypot.Config.Path (Path, evalPath)
import           Honeypot.Prelude
import           Honeypot.Types

data Config a where
  CBoard   :: [Pos] -> Config Board
  CEnemies :: [Path] -> Config [Enemy]
  CPlayer  :: Dir -> Pos -> Fuel -> Config Player
  CEnv     :: Config Board -> Config [Enemy] -> Config Player -> Config Env

data ConfigError = BlockOutOfBounds Pos
                 | EnemyPathIsNotStraightLines
                 | NoPointsInEnemyPath
                 | PlayerOutOfBounds Pos
                 | NegativePlayerFuel Fuel

newtype ConfigEval a = ConfigEval { runConfEval :: ReaderT Dim (Except ConfigError) a }
  deriving (Functor, Applicative, Monad, MonadReader Dim, MonadError ConfigError)

evalConfig :: Config a -> ConfigEval a
evalConfig (CPlayer dir pos fuel) = do
  d <- ask
  if | outOfBounds d pos -> throwError (PlayerOutOfBounds pos)
     | fuel < 0          -> throwError (NegativePlayerFuel fuel)
     | otherwise         -> return (Player dir pos fuel)
evalConfig (CBoard ps) = do
  (y,x) <- ask
  let m = matrix y x $ \_ -> False
      f m p = setElem True p m
  case find (outOfBounds (y,x)) ps of
    Just p  -> throwError (BlockOutOfBounds p)
    Nothing -> return $ Board (y,x) (foldl' f m ps)
evalConfig (CEnemies paths) = do
  d <- ask
  case traverse (evalPath d) paths of
    Nothing     -> throwError EnemyPathIsNotStraightLines
    Just paths' -> traverse evalEnemy paths'
evalConfig (CEnv board enemies player) =
  Env <$> evalConfig board <*> evalConfig enemies <*> evalConfig player

evalEnemy :: [Pos] -> ConfigEval Enemy
evalEnemy [] = throwError NoPointsInEnemyPath
evalEnemy (p:ps) = return E { future = ps
                            , current = p
                            , past = []
                            }
