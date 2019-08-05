{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Config.Env where

import           Control.Monad.Except (Except, MonadError, throwError)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, runReader)
import qualified Data.Map             as Map (fromList)
import qualified Data.Matrix          as Matrix (fromList, matrix, setElem)
import qualified Data.Set             as Set (fromList)
import           Honeypot.Config.Path (Path, evalPath)
import           Honeypot.Prelude
import           Honeypot.Types

-- TODO this should go in Env
data Player = P Fuel Pos Dir

data Config a where
  CBoard   :: [Pos] -> Config Board
  CEnemies :: [Path] -> Config (Set Enemy)
  CPlayer  :: Fuel -> Pos -> Dir -> Config Player
  CEnv     :: Config Board -> Config (Set Enemy) -> Config Player -> Config Env

data ConfigError = BlockOutOfBounds Pos
                 | EnemyPathIsNotStraightLines
                 | NoPointsInEnemyPath
                 | PlayerOutOfBounds Pos
                 | NegativePlayerFuel Fuel

newtype ConfigEval a = ConfigEval { runConfEval :: ReaderT Dim (Except ConfigError) a }
  deriving (Functor, Applicative, Monad, MonadReader Dim, MonadError ConfigError)

evalConfig :: Config a -> ConfigEval a
evalConfig (CPlayer fuel pos dir) = do
  d <- ask
  if | outOfBounds d pos -> throwError (PlayerOutOfBounds pos)
     | fuel < 0          -> throwError (NegativePlayerFuel fuel)
     | otherwise         -> return (P fuel pos dir)
evalConfig (CBoard ps) = do
  (y,x) <- ask
  let m = Matrix.matrix y x $ \_ -> Nothing
      f m p = Matrix.setElem (Just B) p m
  case find (outOfBounds (y,x)) ps of
    Just p  -> throwError (BlockOutOfBounds p)
    Nothing -> return $ Board (y,x) (foldl' f m ps)
evalConfig (CEnemies paths) = do
  d <- ask
  case traverse (evalPath d) paths of
    Nothing     -> throwError EnemyPathIsNotStraightLines
    Just paths' -> Set.fromList <$> traverse evalEnemy paths'
evalConfig (CEnv board enemies player) = do
  b <- evalConfig board
  e <- evalConfig enemies
  p <- evalConfig player
  return undefined

evalEnemy :: [Pos] -> ConfigEval Enemy
evalEnemy [] = throwError NoPointsInEnemyPath
evalEnemy ps@(p:ps') = return (E p Forward f)
  where
    f = undefined

pathEvents :: [(Pos, Pos)] -> Map Pos PathEvent
pathEvents []   = empty
pathEvents [pp] = Map.fromList [start pp, end pp]
pathEvents pps  = Map.fromList $ start x : (move <$> y) ++ [end z]
  where
    x = head pps
    y = tail pps
    z = last pps

start, move, end :: (Pos, Pos) -> (Pos, PathEvent)
start ps@(p,_) = (p, Start (pathDir' ps))
move ps@(p,_) = (p, Move (pathDir' ps))
end ps@(_,p) = (p, End (pathDir' ps))

pathDir' :: (Pos, Pos) -> Dir
pathDir' ((y1,x1), (y2,x2))
  | y1 < y2 && x1 == x2 = South
  | y1 > y2 && x1 == x2 = North
  | y1 == y2 && x1 < x2 = East
  | y1 == y2 && x1 > x2 = West
