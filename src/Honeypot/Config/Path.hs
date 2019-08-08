{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Honeypot.Config.Path
  ( Path
  , go
  , end
  , evalPath
  ) where

import           Control.Monad.Free (Free (..), liftF)
import           Honeypot.Prelude
import           Honeypot.Types     (Dim, Pos, outOfBounds)

data Walk next = Go Pos next
               | End
  deriving Functor

type Path = Free Walk ()

go :: Pos -> Path
go p = liftF (Go p ())

end :: Path
end = liftF End

-- example
path :: Path
path = do
  go (10, 2)
  end

evalPath :: Dim -> Path -> Maybe [Pos]
evalPath dim (Pure ())   = Just []
evalPath dim (Free path) =
  case path of
    Go pos next -> do ps <- evalPath dim next
                      match dim pos ps
    End -> Just []

match :: Dim -> Pos -> [Pos] -> Maybe [Pos]
match dim (y1,x1) ((y2,x2):ps)
  | outOfBounds dim (y1,x1) = Nothing
  | x1 == x2 && y1 < y2     = Just $ zip [y1..y2] (repeat x1) ++ ps
  | x1 == x2 && y1 > y2     = Just $ zip [y1,y1-1..y2] (repeat x1) ++ ps
  | x1 < x2 && y1 == y2     = Just $ zip (repeat y1) [x1..x2] ++ ps
  | x1 > x2 && y1 == y2     = Just $ zip (repeat y1) [x1,x1-1..x2] ++ ps
  | otherwise               = Nothing
match d p [] = Just [p]
