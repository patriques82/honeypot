{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Config.Env where

import           Control.Monad.Except (Except, MonadError, throwError)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, runReader)
import           Data.Matrix          (matrix, setElem)
import           Honeypot.Config.Path (Path, evalP)
import           Honeypot.Prelude
import           Honeypot.Types

-- TODO this should go in Env
data Player = P Fuel Pos Dir

data Config a where
  CBoard   :: [Pos] -> Config Board
  CEnemies :: [Path] -> Config (Set Enemy)
  CPlayer  :: Fuel -> Pos -> Dir -> Config Player
  CEnv     :: Config Board -> Config (Set Enemy) -> Config Player -> Config Env

data ConfigError = BlockOutOfBounds
                 | EnemyPathIsNotStraightLines
                 | NoPointsInEnemyPath

newtype ConfigEval a = ConfigEval { runConfEval :: ReaderT Dim (Except ConfigError) a }
  deriving (Functor, Applicative, Monad, MonadReader Dim, MonadError ConfigError)

evalC :: Config a -> ConfigEval a
evalC (CPlayer fuel pos dir) =
  return (P fuel pos dir)
evalC (CBoard ps) = do
  (y,x) <- ask
  let init = matrix y x $ \_ -> Nothing
      f m p = setElem (Just B) p m
  case find (outOfBounds (y,x)) ps of
    Just p  -> throwError BlockOutOfBounds
    Nothing -> return $ Board (y,x) (foldl' f init ps)
evalC (CEnemies paths) = do
  d <- ask
  case traverse (evalP d) paths of
    Nothing     -> throwError EnemyPathIsNotStraightLines
    Just paths' -> fromList <$> traverse evalEnemy paths'
evalC (CEnv board enemies player) = do
  b <- evalC board
  e <- evalC enemies
  p <- evalC player
  return undefined

evalEnemy :: [Pos] -> ConfigEval Enemy
evalEnemy [] = throwError NoPointsInEnemyPath
evalEnemy ps@(p:ps') = return (E p Forward f)
  where
    xs = zip ps ps' -- could be one point
    x = head xs -- this is always ok by def
    y = tail xs -- this could error
    z = last xs -- this could error
    path' = start x : (move <$> y) ++ [end z]
    f dir pos = undefined

start, move, end :: (Pos, Pos) -> (Pos, PathEvent)
start ps@(p,_) = (p, Start (pathDir' ps))
move ps@(p,_) = (p, Move (pathDir' ps))
end ps@(p,_) = (p, End (pathDir' ps))

pathDir' :: (Pos, Pos) -> Dir
pathDir' ((y1,x1), (y2,x2))
  | y1 < y2 && x1 == x2 = South
  | y1 > y2 && x1 == x2 = North
  | y1 == y2 && x1 < x2 = East
  | y1 == y2 && x1 > x2 = West
