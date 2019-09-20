module Main where

import           Graphics                     (draw)
import           Graphics.Gloss               (Display (InWindow), circle,
                                               display, loadBMP, simulate,
                                               white)
import           Graphics.Gloss.Data.ViewPort (ViewPort)
import           Honeypot

main :: IO ()
main = do
  tank <- loadBMP "data/tank.bmp"
  monster <- loadBMP "data/monster.bmp"
  case runConfig conf of
    Left e    -> print e
    Right env -> simulate window white simulationRate env (draw tank monster) update

conf :: Config
conf = config { dim = (10, 10)
              , blocks = [ (1,1), (1,7), (1,10)
                         , (2,5), (2,9), (3,1)
                         , (3,2), (4,5), (10,3)
                         ]
              , dir = East
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

window :: Display
window = InWindow "Honeypot" (width, height) (offset, offset)

width = 1100
height = 1100
offset = 100
simulationRate = 1

update :: ViewPort -> Float -> GameState -> GameState
update _ _ state = exec state playerStep

playerStep :: Step Event
playerStep = return Noop

