module Honeypot.Config.Config
  ( module Honeypot.Config.Path
  , Config (..)
  , config
  , runConfig
  ) where

import           Control.Monad.Except (Except, MonadError, runExcept,
                                       throwError)
import           Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import           Honeypot.Config.Env
import           Honeypot.Config.Path
import           Honeypot.Types       (Dir (North), GameState (..), position)

data Config = Config { dim     :: (Int, Int)
                     , blocks  :: [(Int, Int)]
                     , dir     :: Dir
                     , pos     :: (Int, Int)
                     , fuel    :: Int
                     , enemies :: [Path]
                     }

config :: Config
config = Config { dim = (0,0)
                , blocks = []
                , dir = North
                , pos = (0,0)
                , fuel = 0
                , enemies = []
                }

runConfig :: Config -> Either ConfigError GameState
runConfig (Config dim b dir p f e) =
  Continue <$> runExcept (runReaderT (runConfEval (evalConfig conf)) (position dim))
    where
      conf = CEnv (CBoard (fmap position b)) (CEnemies e) (CPlayer dir (position p) f)
