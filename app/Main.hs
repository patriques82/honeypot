import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

import Lib


width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Honeypot" (width, height) (offset, offset)

computerStep = undefined

main :: IO ()
main = simulate 
  window
  white
  simulationRate
  initEnv
  draw
  update
  where
    simulationRate :: Int
    simulationRate = 20

    initEnv :: Environment
    initEnv = undefined

    draw :: Environment -> Picture
    draw e = undefined

    update :: ViewPort -> Float -> Environment -> Environment
    update _ dt = snd . runStep computerStep
