{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Honeypot.Api.EventCalc where

import qualified Control.Monad.State as ST
import           Honeypot.Prelude
import           Honeypot.Types

newtype EventCalc a = EventCalc { runCalc :: ST.StateT Env (Either Status) a }
  deriving (Functor, Applicative, Monad, ST.MonadState Env)

lift :: Either Status a -> EventCalc a
lift e = EventCalc $ ST.lift e

put :: Env -> EventCalc ()
put = ST.put

get :: EventCalc Env
get = ST.get

modify :: (Env -> Env) -> EventCalc ()
modify = ST.modify

execEventCalc :: EventCalc a -> Env -> Either Status Env
execEventCalc calc = ST.execStateT (runCalc calc)
