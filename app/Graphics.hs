module Graphics
  ( draw
  ) where

import           Graphics.Gloss
import           Honeypot       (Board, Enemy (..), Env (..), GameState (..),
                                 Player (..), Pos (..), Status (..),
                                 getOccupied)

draw :: GameState -> Picture
draw (GameOver Lost) = undefined
draw (GameOver Won) = undefined
draw (Continue (Env b es p)) = rotate 90.0 (Pictures [ board b
                                                     , enemies es
                                                     , player p
                                                     ])

board :: Board -> Picture
board b = Pictures $ rows ++ cols ++ (blocks b)

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

enemies :: [Enemy] -> Picture
enemies = Pictures . foldr (\(E _ (P y x) _) xs ->
  translate (convert x) (convert y) (color red (circleSolid 15)) : xs) []

player :: Player -> Picture
player (Player _ (P y x) _) =
  translate (convert x) (convert y) (color blue (circleSolid 15))

convert :: Int -> Float
convert x = fromIntegral x * 30.0 - 15.0
