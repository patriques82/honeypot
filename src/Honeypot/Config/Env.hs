{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Config.Env where

import           Control.Monad.Except (Except, MonadError, runExcept,
                                       throwError)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import           Data.Matrix          (Matrix, fromList, matrix, setElem)
import           Honeypot.Config.Path (Path, evalPath)
import           Honeypot.Config.Util
import           Honeypot.Prelude
import           Honeypot.Types       (Dim, Dir (..), Enemy (..), Env (..),
                                       Fuel, Player (..), Pos (..))

data EnvConfig a where
  CBoard   :: [Pos] -> EnvConfig (Matrix Bool)
  CEnemies :: [Path] -> EnvConfig [Enemy]
  CPlayer  :: Dir -> Pos -> Fuel -> EnvConfig Player
  CEnv     :: EnvConfig (Matrix Bool) -> EnvConfig [Enemy] -> EnvConfig Player -> EnvConfig Env

data ConfigError = BlockOutOfBounds Pos
                 | EnemyPathIsNotStraightLines
                 | NoPointsInEnemyPath
                 | PlayerOutOfBounds Pos
                 | NegativePlayerFuel Fuel
                 deriving Show

newtype ConfigEval a = ConfigEval { runConfEval :: ReaderT Dim (Except ConfigError) a }
  deriving (Functor, Applicative, Monad, MonadReader Dim, MonadError ConfigError)


evalConfig :: EnvConfig a -> ConfigEval a
evalConfig (CPlayer dir pos fuel) = do
  d <- ask
  if | outOfBounds d pos -> throwError (PlayerOutOfBounds pos)
     | fuel < 0          -> throwError (NegativePlayerFuel fuel)
     | otherwise         -> return (Player dir pos fuel)
evalConfig (CBoard ps) = do
  (P y x) <- ask
  let m = matrix y x $ const False
      f m (P y x) = setElem True (y,x) m
  case find (outOfBounds (P y x)) ps of
    Just p  -> throwError (BlockOutOfBounds p)
    Nothing -> return (foldl' f m ps)
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
