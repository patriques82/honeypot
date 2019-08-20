module Main where

import           Data.Matrix
import           Graphics.Gloss               hiding (dim)
import           Graphics.Gloss.Data.Bitmap
import           Graphics.Gloss.Data.ViewPort

import           Honeypot

conf :: Config
conf = config { dim = (10, 10)
                 , blocks = [ (1,1), (1,7), (1,10)
                            , (2,5), (2,9), (3,1)
                            , (3,2), (4,5), (10,3)
                            ]
                 , dir = North
                 , pos = (3,3)
                 , fuel = 100
                 , enemies = [ do go 1 2
                                  go 2 2
                                  go 2 4
                                  end
                             , do go 10 2
                                  end
                             ]
                 }

playerStep :: Step Event
playerStep = return Noop

-- resolve and apply player step
update :: ViewPort -> Float -> GameState -> GameState
update _ _ state = exec state playerStep

main :: IO ()
main = do
  case runConfig conf of
    Left e    -> putStrLn (show e)
    Right env -> simulate window white simulationRate env draw update

-- drawing

width, height, offset :: Int
width = 1100
height = 1100
offset = 100

window :: Display
window = InWindow "Honeypot" (width, height) (offset, offset)

simulationRate :: Int
simulationRate = 1

board :: Matrix Bool -> Picture
board m = Pictures ps
  where
    ps = [ -- rows
           color black (line [(0.0, 0.0), (300.0, 0.0)])
         , color black (line [(0.0, 30.0), (300.0, 30.0)])
         , color black (line [(0.0, 60.0), (300.0, 60.0)])
         , color black (line [(0.0, 90.0), (300.0, 90.0)])
         , color black (line [(0.0, 120.0), (300.0, 120.0)])
         , color black (line [(0.0, 150.0), (300.0, 150.0)])
         , color black (line [(0.0, 180.0), (300.0, 180.0)])
         , color black (line [(0.0, 210.0), (300.0, 210.0)])
         , color black (line [(0.0, 240.0), (300.0, 240.0)])
         , color black (line [(0.0, 270.0), (300.0, 270.0)])
         , color black (line [(0.0, 300.0), (300.0, 300.0)])
         -- cols
         , color black (line [(0.0, 0.0), (0.0, 300.0)])
         , color black (line [(30.0, 0.0), (30.0, 300.0)])
         , color black (line [(60.0, 0.0), (60.0, 300.0)])
         , color black (line [(90.0, 0.0), (90.0, 300.0)])
         , color black (line [(120.0, 0.0), (120.0, 300.0)])
         , color black (line [(150.0, 0.0), (150.0, 300.0)])
         , color black (line [(180.0, 0.0), (180.0, 300.0)])
         , color black (line [(210.0, 0.0), (210.0, 300.0)])
         , color black (line [(240.0, 0.0), (240.0, 300.0)])
         , color black (line [(270.0, 0.0), (270.0, 300.0)])
         , color black (line [(300.0, 0.0), (300.0, 300.0)])
         -- blocks
         ] ++ (blocks' m)

blocks' :: Matrix Bool -> [Picture]
blocks' = fmap f . getOccupied
  where f (P y x) = color black $ translate (fromIntegral x * 30.0 - 15.0) (fromIntegral y * 30.0 - 15.0) $ rectangleSolid 30.0 30.0

getOccupied :: Matrix Bool -> [Pos]
getOccupied m = foldr (\(y,x) xs -> if m ! (y,x) then ((P y x) : xs) else xs) [] ps
  where ps = [(y',x') | y' <- [1..(ncols m)] , x' <- [1..(nrows m)]]

redBlocks :: [Enemy] -> Picture
redBlocks = Pictures . foldr (\(E _ (P y x) _) xs ->
  translate (fromIntegral x * 30.0 - 15.0) (fromIntegral y * 30.0 - 15.0) (color red (circleSolid 15)) : xs) []

player :: Player -> Picture
player (Player _ (P y x) _) =
  translate (fromIntegral x * 30.0 - 15.0) (fromIntegral y * 30.0 - 15.0) (color blue (circleSolid 15))

-- bulk of work
draw :: GameState -> Picture
draw (GameOver status)       = undefined
draw (Continue (Env b es p)) = rotate 90.0 (Pictures [ board b
                                                     , redBlocks es
                                                     , player p
                                                     ])

