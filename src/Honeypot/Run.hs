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
runGame step conf@C.Config {..} =
  case C.runConfig conf of
    Right state -> runSim state dim gfxCtx step
    Left err    -> print err

runSim :: GameState -> (Int, Int) -> C.GfxCtx -> Step Event -> IO ()
runSim state dim gfxCtx step = do
  picCfg <- mkPictureCfg dim gfxCtx
  simulate win bg freq state (draw picCfg) (update step)
   where
     win = InWindow "Honeypot Challenge" (winSize dim) (0,0)
     update step _ _ state = exec state step
     bg = white
     freq = 1

winSize :: (Int, Int) -> (Int, Int)
winSize (x, y) = (x * cell * 4, y * cell * 3)
  where
    cell = round cellSize

mkPictureCfg :: (Int, Int) -> C.GfxCtx -> IO PictureCfg
mkPictureCfg dim C.GfxCtx {..} = do
  tank <- loadBMP tankBmp
  enemy <- loadBMP enemyBmp
  return PictureCfg {..}
