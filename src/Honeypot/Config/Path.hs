{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Honeypot.Config.Path
  ( Path
  , go
  , end
  , evalPath
  ) where

import           Control.Monad.Free   (Free (..), liftF)
import           Honeypot.Config.Util
import           Honeypot.Prelude
import           Honeypot.Types

data Walk next = Go Pos next
               | End
  deriving Functor

type Path = Free Walk ()

-- x y
go :: Int -> Int -> Path
go x y = liftF (Go (P y x) ())

end :: Path
end = liftF End

evalPath :: Dim -> Path -> Maybe [Pos]
evalPath dim (Pure ())   = Just []
evalPath dim (Free path) =
  case path of
    End         -> Just []
    Go pos next -> do ps <- evalPath dim next
                      match dim pos ps

match :: Dim -> Pos -> [Pos] -> Maybe [Pos]
match dim (P y1 x1) ((P y2 x2):ps)
  | outOfBounds dim (P y1 x1) = Nothing
  | x1 == x2 && y1 < y2 = Just $ zipWith P [y1..y2] (repeat x1) ++ ps
  | x1 == x2 && y1 > y2 = Just $ zipWith P [y1,y1-1..y2] (repeat x1) ++ ps
  | x1 < x2 && y1 == y2 = Just $ zipWith P (repeat y1) [x1..x2] ++ ps
  | x1 > x2 && y1 == y2 = Just $ zipWith P (repeat y1) [x1,x1-1..x2] ++ ps
  | otherwise           = Nothing
match dim pos []
  | outOfBounds dim pos = Nothing
  | otherwise           = Just [pos]

