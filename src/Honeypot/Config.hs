{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Honeypot.Config.Env where

import           Data.Matrix          (matrix, setElem)
import           Honeypot.Config.Path (Path, evalP)
import           Honeypot.Prelude
import           Honeypot.Types

data Player = P Fuel Pos Dir

data Config a where
  CBoard   :: Dim -> [Pos] -> Config Board
  CEnemies :: [Path] -> Config (Set Enemy)
  CPlayer  :: Fuel -> Pos -> Dir -> Config Player
  CEnv     :: Config Board -> Config (Set Enemy) -> Config Player -> Config Env

data ConfigError = BlockOutOfBounds
                 | EnemyPathError
                 | NoPointInPath

evalC :: Config a -> Either ConfigError a
evalC (CPlayer fuel pos dir)      = Right (P fuel pos dir)
evalC (CBoard (y,x) ps)           =
  let init = matrix y x $ \_ -> Nothing
      f m p = setElem (Just B) p m
   in case find (outOfBounds (y,x)) ps of -- use traverse instead
        Nothing -> Right $ Board (y,x) (foldl' f init ps)
        Just p  -> Left BlockOutOfBounds
evalC (CEnemies paths)            =
  case traverse evalP paths of
    Nothing     -> Left EnemyPathError
    Just paths' -> fromList <$> traverse evalEnemy paths'
evalC (CEnv board enemies player) = undefined -- no collision between player and cells, and

-- smart constructor
evalEnemy :: [Pos] -> Either ConfigError Enemy
evalEnemy [] = Left NoPointInPath
evalEnemy ps@(p:ps') = Right (E p Forward f)
  where
    xs = zip ps ps'
    h = head xs
    t = last xs
    path' = start h : (path <$> (tail xs)) ++ [end t]
    f dir pos = undefined

start :: (Pos, Pos) -> (Pos, PathEvent)
start (p1,p2) = (p1, f p1 p2)
  where f (y1,x1) (y2,x2)
          | y1 < y2 && x1 == x2 = Start South
          | y1 > y2 && x1 == x2 = Start North
          | y1 == y2 && x1 < x2 = Start East
          | y1 == y2 && x1 > x2 = Start West


path :: (Pos, Pos) -> (Pos, PathEvent)
path (p1,p2) = (p1, f p1 p2)
  where f (y1,x1) (y2,x2)
          | y1 < y2 && x1 == x2 = Move South
          | y1 > y2 && x1 == x2 = Move North
          | y1 == y2 && x1 < x2 = Move East
          | y1 == y2 && x1 > x2 = Move West

end :: (Pos, Pos) -> (Pos, PathEvent)
end (p1,p2) = (p2, f p1 p2)
  where f (y1,x1) (y2,x2)
          | y1 < y2 && x1 == x2 = End South
          | y1 > y2 && x1 == x2 = End North
          | y1 == y2 && x1 < x2 = End East
          | y1 == y2 && x1 > x2 = End West
