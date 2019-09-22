{-# LANGUAGE RecordWildCards #-}

module Honeypot.Config.Config
  ( module Honeypot.Config.Path
  , Config (..)
  , GfxCtx (..)
  , config
  , context
  , runConfig
  ) where

import           Control.Monad.Except (Except, MonadError, runExcept,
                                       throwError)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import           Honeypot.Config.Env
import           Honeypot.Config.Path
import           Honeypot.Prelude
import           Honeypot.Types       (Dir (North), GameState (..), position)

data GfxCtx = GfxCtx { tankBmp  :: FilePath
                     , enemyBmp :: FilePath
                     , size     :: (Int, Int)
                     }

data Config = Config { dim     :: (Int, Int)
                     , blocks  :: [(Int, Int)]
                     , dir     :: Dir
                     , pos     :: (Int, Int)
                     , fuel    :: Int
                     , enemies :: [Path]
                     , gfxCtx  :: GfxCtx
                     }

context :: GfxCtx
context = GfxCtx { tankBmp = ""
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
                , gfxCtx = context
                }

runConfig :: Config -> Either ConfigError GameState
runConfig conf@Config {..} =
  Continue <$> runExcept (runReaderT (runConfEval (evalConfig conf)) (position dim))
    where
      conf = CEnv (CBoard (fmap position blocks)) (CEnemies enemies) (CPlayer dir (position pos) fuel)
