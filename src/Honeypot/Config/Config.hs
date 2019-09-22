{-# LANGUAGE RecordWildCards #-}

module Honeypot.Config.Config
  ( module Honeypot.Config.Path
  , Config (..)
  , GraphicsCfg (..)
  , config
  , graphics
  , runConfig
  ) where

import           Control.Monad.Except (Except, MonadError, runExcept,
                                       throwError)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import           Honeypot.Config.Env
import           Honeypot.Config.Path
import           Honeypot.Prelude
import           Honeypot.Types       (Dir (North), GameState (..), position)

data GraphicsCfg = GraphicsCfg { tankBmp  :: FilePath
                               , enemyBmp :: FilePath
                               , size     :: (Int, Int)
                               }

data Config = Config { dim         :: (Int, Int)
                     , blocks      :: [(Int, Int)]
                     , dir         :: Dir
                     , pos         :: (Int, Int)
                     , fuel        :: Int
                     , enemies     :: [Path]
                     , graphicsCfg :: GraphicsCfg
                     }

graphics :: GraphicsCfg
graphics = GraphicsCfg { tankBmp = ""
                       , enemyBmp = ""
                       , size = (0,0)
                       }

config :: Config
config = Config { dim = (0,0)
                , blocks = []
                , dir = North
                , pos = (0,0)
                , fuel = 0
                , enemies = []
                , graphicsCfg = graphics
                }

runConfig :: Config -> Either ConfigError GameState
runConfig conf@Config {..} =
  Continue <$> runExcept (runReaderT (runConfEval (evalConfig conf)) (position dim))
    where
      conf = CEnv cBoard cEnemies cPlayer
      cBoard = CBoard (fmap position blocks)
      cEnemies = CEnemies enemies
      cPlayer = CPlayer dir (position pos) fuel
