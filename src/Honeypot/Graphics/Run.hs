{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Honeypot.Graphics.Run
  ( runAnimation
  , GraphicsCfg (..)
  ) where

import qualified Graphics.Gloss                       as G
import           Graphics.Gloss.Interface.Environment (getScreenSize)
import qualified Honeypot.Config.Config               as C
import           Honeypot.Core.Api                    (exec)
import           Honeypot.Graphics.Draw
import           Honeypot.Prelude
import           Honeypot.Types

type BoardDim = (Int, Int)
type ScreenDim = (Int, Int)

data GraphicsCfg = GraphicsCfg { tankBmp  :: FilePath
                               , enemyBmp :: FilePath
                               , size     :: (Int, Int)
                               }


runAnimation :: Step Event -> C.Config -> GraphicsCfg -> IO ()
runAnimation step conf graphicsCfg =
  case C.runConfig conf of
    Right state -> run state (C.dim conf) graphicsCfg step
    Left err    -> print err

run :: GameState -> (Int, Int) -> GraphicsCfg -> Step Event -> IO ()
run state boardDim ctx step = do
  screenDim <- getScreenSize
  picConf <- mkPictureConf boardDim ctx
  G.simulate win G.white fps state (draw picConf) (update step)
   where
     win = G.FullScreen
     --win = InWindow "Honeypot Challenge" (winSize screenDim boardDim) (0,0)
     update step _ _ state = exec state step
     fps = 1

mkPictureConf :: BoardDim -> GraphicsCfg -> IO PictureConf
mkPictureConf dim GraphicsCfg {..} = do
  tank <- G.loadBMP tankBmp
  enemy <- G.loadBMP enemyBmp
  return PictureConf {..}

winSize :: ScreenDim -> BoardDim -> (Int, Int)
winSize (sw, sh) (bw, bh) = undefined
  where
    cell = round cellSize
