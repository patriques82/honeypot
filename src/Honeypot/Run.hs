{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Honeypot.Run
  ( runGame
  ) where

import           Graphics.Gloss
import qualified Honeypot.Config.Config as C
import           Honeypot.Core.Api      (exec)
import           Honeypot.Graphics.Draw
import           Honeypot.Prelude
import           Honeypot.Types

runGame :: Step Event -> C.Config -> IO ()
runGame step conf@C.Config {dim, graphicsCfg, ..} =
  case C.runConfig conf of
    Right state -> run state dim graphicsCfg step
    Left err    -> print err

run :: GameState -> (Int, Int) -> C.GraphicsCfg -> Step Event -> IO ()
run state dim ctx step = do
  picConf <- mkPictureConf dim ctx
  simulate win white fps state (draw picConf) (update step)
   where
     win = InWindow "Honeypot Challenge" (winSize dim) (0,0)
     update step _ _ state = exec state step
     fps = 1

mkPictureConf :: (Int, Int) -> C.GraphicsCfg -> IO PictureConf
mkPictureConf dim C.GraphicsCfg {..} = do
  tank <- loadBMP tankBmp
  enemy <- loadBMP enemyBmp
  return PictureConf {..}

winSize :: (Int, Int) -> (Int, Int)
winSize (x, y) = (x * cell * 4, y * cell * 3)
  where
    cell = round cellSize
