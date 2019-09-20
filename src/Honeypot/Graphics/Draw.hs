module Honeypot.Graphics.Draw
  ( draw
  ) where

import           Data.Matrix
import           Graphics.Gloss
import           Honeypot.Types

draw :: Picture -> Picture -> GameState -> Picture
draw _ _ (GameOver Lost) = undefined
draw _ _ (GameOver Won) = undefined
draw tank monster (Continue (Env b es p)) =
  rotate 90.0 (Pictures [ drawBoard b
                        , drawEnemies monster es
                        , drawPlayer tank p
                        ])

drawBoard :: Board -> Picture
drawBoard b = Pictures $ rows ++ cols ++ blocks b

rows :: [Picture]
rows = [ color black (line [x, y]) | (x, y) <- xs `zip` ys ]
  where
    xs = zip [0.0, 0.0 .. 0.0] [0.0, 30.0 .. 300.0]
    ys = zip [300.0, 300.0 .. 300.0] [0.0, 30.0 .. 300.0]

cols :: [Picture]
cols = [ color black (line [x, y]) | (x, y) <- xs `zip` ys ]
  where
    xs = zip [0.0, 30.0 .. 300.0] [0.0, 0.0 .. 0.0]
    ys = zip [0.0, 30.0 .. 300.0] [300.0, 300.0 .. 300.0]

blocks :: Board -> [Picture]
blocks = fmap f . getOccupied
  where f (P y x) = translate (convert x) (convert y) $ color black (rectangleSolid 30.0 30.0)

drawEnemies :: Picture -> [Enemy] -> Picture
drawEnemies monster = Pictures . foldr (\(E _ (P y x) _) xs ->
  translate (convert x) (convert y) (scale 0.1 0.1 monster) : xs) []

drawPlayer :: Picture -> Player -> Picture
drawPlayer tank (Player dir (P y x) _) =
  translate (convert x) (convert y) (rotate (dir2Deg dir) (scale 0.1 0.1 tank))

convert :: Int -> Float
convert x = fromIntegral x * 30.0 - 15.0

dir2Deg :: Dir -> Float
dir2Deg North = 270.0
dir2Deg South = 90.0
dir2Deg East  = 0.0
dir2Deg West  = 180.0

getOccupied :: Board -> [Pos]
getOccupied b = foldr (\(y,x) xs -> if occupied b (y,x) then P y x : xs else xs) [] ps
  where ps = [(y',x') | y' <- [1..(ncols b)] , x' <- [1..(nrows b)]]

occupied :: Board -> (Int, Int) -> Bool
occupied = (!)
