{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Honeypot.Config.Env where

import           Data.Matrix          (matrix, setElem)
import           Honeypot.Config.Path (Path, evalP)
import           Honeypot.Prelude
import           Honeypot.Types

data Player = P Fuel Pos Dir

data Config a where
  CBoard   :: [Pos] -> Config Board
  CEnemies :: [Path] -> Config (Set Enemy)
  CPlayer  :: Fuel -> Pos -> Dir -> Config Player
  CEnv     :: Config Board -> Config (Set Enemy) -> Config Player -> Config Env

data ConfigError = BlockOutOfBounds
                 | EnemyPathIsNotStraightLines
                 | NoPointsInEnemyPath

evalC :: Dim -> Config a -> Either ConfigError a
evalC (y,x) (CPlayer fuel pos dir)      = Right (P fuel pos dir)
evalC (y,x) (CBoard ps)                 =
  let init = matrix y x $ \_ -> Nothing
      f m p = setElem (Just B) p m
   in case find (outOfBounds (y,x)) ps of -- use traverse instead
        Nothing -> Right $ Board (y,x) (foldl' f init ps)
        Just p  -> Left BlockOutOfBounds
evalC d (CEnemies paths)            =
  case traverse (evalP d) paths of
    Nothing     -> Left EnemyPathIsNotStraightLines
    Just paths' -> fromList <$> traverse evalEnemy paths'
evalC d (CEnv board enemies player) = undefined -- no collision between player and cells, and

-- smart constructor
evalEnemy :: [Pos] -> Either ConfigError Enemy
evalEnemy [] = Left NoPointsInEnemyPath
evalEnemy ps@(p:ps') = Right (E p Forward f)
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
