module Lib
    ( someFunc
    ) where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Loops (iterateUntilM)
import Data.Either (isLeft)
import Prelude as P

someFunc :: IO ()
someFunc = putStrLn "someFunc"


{--# Plan

Player = the programmable player (handler), contains id, postion and action

Enemy = contains id, position and action

#--}


-- Types
data Position = Pos Int Int

data Direction = Left | Right | Up | Down

data Entity = Entity { life :: Integer
                     , pos :: Position
                     , dir :: Direction
                     }

data Event = ChangeDir Entity Direction
           | Move Entity Direction
           | Shoot Entity Direction

data Config = Config { obstacles :: [Position]
                     , enemies :: [Entity]
                     , player :: Entity
                     }


-- Combinators
type Step m a = ReaderT Config (WriterT [Event] m) a

config :: Monad m => Step m Config
config = ask

push :: Monad m => Event -> Step m ()
push evt = tell [evt]

runStep :: Monad m => Step m a -> Config -> m (a, [Event])
runStep step = runWriterT . runReaderT step



-- Run
merge :: Monad m => Config -> [Event] -> m Config
merge conf evs = undefined

ended :: Config -> Bool
ended conf = undefined

runGame :: Monad m => Step m Config -> Config -> m Config
runGame step conf =
  iterateUntilM ended ((=<<) (uncurry merge) . runStep step) conf
   
-- runGame step c = do
  -- e <- runStep step c
  -- case e of
    -- P.Left end -> return end




