module Honeypot.Run
  ( runGame
  ) where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort (ViewPort)
import           Honeypot.Config.Config       (Config, enemyBmp, runConfig,
                                               tankBmp)
import           Honeypot.Core.Api            (exec)
import           Honeypot.Graphics.Draw       (draw)
import           Honeypot.Types

runGame :: Step Event -> Config -> IO ()
runGame step conf = do
  tank <- loadBMP (tankBmp conf)
  monster <- loadBMP (enemyBmp conf)
  case runConfig conf of
    Left err    -> print err
    Right state -> runSim state tank monster step

runSim :: GameState -> Picture -> Picture -> Step Event -> IO ()
runSim state tank monster step =
  simulate window white simulationRate state (draw tank monster) (update step)

window :: Display
window = InWindow "Honeypot" (width, height) (offset, offset)

width = 1100
height = 1100
offset = 100
simulationRate = 1

update :: Step Event -> ViewPort -> Float -> GameState -> GameState
update step _ _ state = exec state step
