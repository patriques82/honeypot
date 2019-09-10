module Honeypot.Graphics where

import           Data.Matrix
import           Honeypot.Types

data Pair = Pair Pos Pos

data Shape = Line Pair
           | Rect Pair
           | Lines [Pair]
           | Circle Pos Int
           | Shapes [Shape]

class Drawable a where
  draw :: a -> Int -> Int -> Shape

instance Drawable Env where
  draw (Env t e p) y x = undefined --Shapes [draw t y x, draw e y' x', draw p y' x']
    --where y' = y / ncols t
          --x' = x / nrows t

instance Drawable Player where
  draw (Player _ p _) y x = undefined --Rect (Pair p (p + (P y x)))

instance Drawable Enemy where
  draw (E _ p _) y x = undefined -- Rect (Pair p (p + (P (y -  x)))

instance Drawable (Matrix a) where
  draw m p = undefined

getOccupied :: Board -> [Pos]
getOccupied m = foldr (\(y,x) xs -> if occupied m (y,x) then ((P y x) : xs) else xs) [] ps
  where ps = [(y',x') | y' <- [1..(ncols m)] , x' <- [1..(nrows m)]]

occupied :: Board -> (Int, Int) -> Bool
occupied = (!)
