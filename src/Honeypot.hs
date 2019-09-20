module Honeypot
  ( module Honeypot.Api
  , module Honeypot.Config.Config
  , Board
  , Dir (..)
  , Enemy (..)
  , Env (..)
  , Event (..)
  , GameState (..)
  , Player (..)
  , Pos (..)
  , Step
  , Status (..)
  , runGame
  ) where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort (ViewPort)
import           Honeypot.Api
import           Honeypot.Config.Config
import           Honeypot.Graphics
import           Honeypot.Types               (Board, Dir (..), Enemy (..),
                                               Env (..), Event (..),
                                               GameState (..), Player (..),
                                               Pos (..), Status (..), Step)

runGame :: Step Event -> Config -> IO ()
runGame step conf = do
  tank <- loadBMP (tankBmp conf)
  monster <- loadBMP (enemyBmp conf)
  case runConfig conf of
    Left e    -> print e
    Right env -> simulate window white simulationRate env (draw tank monster) (update step)

window :: Display
window = InWindow "Honeypot" (width, height) (offset, offset)

width = 1100
height = 1100
offset = 100
simulationRate = 1

update :: Step Event -> ViewPort -> Float -> GameState -> GameState
update step _ _ state = exec state step
