import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Lib


width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Honeypot" (width, height) (offset, offset)


main :: IO ()
main = simulate 
  window
  white
  simulationRate
  initConfig
  draw
  update
  where
    simulationRate :: Int
    simulationRate = 20

    initConfig :: Config
    initConfig = undefined

    draw :: Config -> Picture
    draw c = undefined

    update :: ViewPort -> Float -> Config -> Config
    update _ dt = snd . runStep computerStep
