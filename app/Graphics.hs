module Graphics
  ( draw
  ) where

import           Graphics.Gloss
import           Honeypot       (Board, Dir (..), Enemy (..), Env (..),
                                 GameState (..), Player (..), Pos (..),
                                 Status (..), getOccupied)

draw :: Picture -> Picture -> GameState -> Picture
draw _ _ (GameOver Lost) = undefined
draw _ _ (GameOver Won) = undefined
draw tank monster (Continue (Env b es p)) =
  rotate 90.0 (Pictures [ board b
                        , enemies monster es
                        , player tank p
                        ])

board :: Board -> Picture
board b = Pictures $ rows ++ cols ++ blocks b

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

enemies :: Picture -> [Enemy] -> Picture
enemies monster = Pictures . foldr (\(E _ (P y x) _) xs ->
  translate (convert x) (convert y) (scale 0.1 0.1 monster) : xs) []

player :: Picture -> Player -> Picture
player tank (Player dir (P y x) _) =
  translate (convert x) (convert y) (rotate (dir2Deg dir) (scale 0.1 0.1 tank))

convert :: Int -> Float
convert x = fromIntegral x * 30.0 - 15.0

dir2Deg :: Dir -> Float
dir2Deg North = 270.0
dir2Deg South = 90.0
dir2Deg East  = 0.0
dir2Deg West  = 180.0
