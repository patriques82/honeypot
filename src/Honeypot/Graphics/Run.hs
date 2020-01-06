{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Honeypot.Graphics.Run
  ( runAnimation
  ) where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Environment (getScreenSize)
import qualified Honeypot.Config.Config               as C
import           Honeypot.Core.Api                    (exec)
import           Honeypot.Graphics.Draw
import           Honeypot.Prelude
import           Honeypot.Types

type BoardDim = (Int, Int)
type ScreenDim = (Int, Int)

runAnimation :: Step Event -> C.Config -> IO ()
runAnimation step conf@C.Config {dim, graphicsCfg, ..} =
  case C.runConfig conf of
    Right state -> run state dim graphicsCfg step
    Left err    -> print err

run :: GameState -> (Int, Int) -> C.GraphicsCfg -> Step Event -> IO ()
run state boardDim ctx step = do
  screenDim <- getScreenSize
  picConf <- mkPictureConf boardDim ctx
  simulate win white fps state (draw picConf) (update step)
   where
     win = FullScreen
     --win = InWindow "Honeypot Challenge" (winSize screenDim boardDim) (0,0)
     update step _ _ state = exec state step
     fps = 1

mkPictureConf :: BoardDim -> C.GraphicsCfg -> IO PictureConf
mkPictureConf dim C.GraphicsCfg {..} = do
  tank <- loadBMP tankBmp
  enemy <- loadBMP enemyBmp
  return PictureConf {..}

winSize :: ScreenDim -> BoardDim -> (Int, Int)
winSize (sw, sh) (bw, bh) = undefined
  where
    cell = round cellSize
