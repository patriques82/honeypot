import Graphics.Gloss




data Model = M (Float, Float)

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)


main :: IO ()
main = simulate 
  window
  white
  simulationRate
  initialModel
  drawingFunc
  updateFunc
  where
    simulationRate :: Int
    simulationRate = 20

    initialModel :: Model
    initialModel = M (0,0)

    drawingFunc :: Model -> Picture
    drawingFunc (M (theta, dtheta)) = Line [(0, 0), (50 * cos theta, 50 * sin theta)]


  --    updateFunc :: ViewPort -> Float -> Model -> Model
    updateFunc _ dt (M (theta, dtheta)) = M (theta + dt * dtheta, dtheta - dt * (cos theta))
