{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Honeypot.Graphics.Draw
  ( cellSize
  , draw
  , PictureConf (..)
  ) where

import           Data.Matrix
import           Graphics.Gloss               hiding (dim)
import           Graphics.Gloss.Data.ViewPort
import           Honeypot.Prelude
import           Honeypot.Types               hiding (Dim)

data PictureConf = PictureConf { tank  :: Picture
                               , enemy :: Picture
                               , dim   :: Dim
                               }

type Dim = (Int, Int)

scaleToViewPort :: Picture -> Picture
scaleToViewPort = applyViewPortToPicture viewPortInit

cellSize :: Float
cellSize = 30.0

draw :: PictureConf -> GameState -> Picture
draw PictureConf {dim, ..} (GameOver Lost) =
  scaleToViewPort (text "You Lost")
draw PictureConf {dim, ..} (GameOver Won) =
  scaleToViewPort (text "You Won")
draw PictureConf {dim, ..} (Continue Env {..}) =
  scaleToViewPort $ rotate 90.0 (Pictures [ drawBoard dim _terrain
                                          , drawEnemies enemy _enemies
                                          , drawPlayer tank _player
                                          ])

drawBoard :: Dim -> Board -> Picture
drawBoard (x,y) b = Pictures $ rows x ++ cols y ++ blocks b

rows :: Int -> [Picture]
rows n = [ color black (line [x, y]) | (x, y) <- xs `zip` ys ]
  where
    h = fromIntegral n * cellSize
    xs = zip [0.0, 0.0 .. 0.0] [0.0, cellSize .. h]
    ys = zip [h, h .. h] [0.0, cellSize .. h]

cols :: Int -> [Picture]
cols m = [ color black (line [x, y]) | (x, y) <- xs `zip` ys ]
  where
    w = fromIntegral m * cellSize
    xs = zip [0.0, cellSize .. w] [0.0, 0.0 .. 0.0]
    ys = zip [0.0, cellSize .. w] [w, w .. w]

blocks :: Board -> [Picture]
blocks = fmap f . getOccupied
  where
    pic = color black (rectangleSolid cellSize cellSize)
    f (P y x) = translate (convert x) (convert y) $ color black pic

drawEnemies :: Picture -> [Enemy] -> Picture
drawEnemies enemy = Pictures . foldr (\(E _ (P y x) _) xs ->
  translate (convert x) (convert y) (scale 0.1 0.1 enemy) : xs) []

drawPlayer :: Picture -> Player -> Picture
drawPlayer tank (Player dir (P y x) _) =
  translate (convert x) (convert y) (rotate (dir2Deg dir) (scale 0.1 0.1 tank))

convert :: Int -> Float
convert x = fromIntegral x * cellSize - (cellSize/2)

dir2Deg :: Dir -> Float
dir2Deg North = 270.0
dir2Deg South = 90.0
dir2Deg East  = 0.0
dir2Deg West  = 180.0
