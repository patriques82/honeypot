{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Prelude as P
import Data.Either (isLeft)
import Control.Lens
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Loops (iterateUntilM)

someFunc :: IO ()
someFunc = putStrLn "someFunc"



-- Types
data Pos = Pos { _x :: Int
               , _y :: Int
               } deriving Eq

data Dir = Left | Up | Right | Down
  deriving (Eq, Ord, Enum, Bounded)


data Variant = Spinner Dir | Walker Pos Pos

data Entity = Entity { _life :: Integer
                     , _pos :: Pos
                     , _dir :: Dir
                     }

instance Eq Entity where
  e1 == e2 = _pos e1 == _pos e2

data Enemy = Enemy { _entity :: Entity
                   , _variant :: Variant
                   } 

instance Eq Enemy where
  e1 == e2 = _entity e1 == _entity e2

data Config = Config { _obstacles :: [Pos]
                     , _enemies :: [Enemy]
                     , _player :: Entity
                     , _dim :: Pos
                     }

data Event = ChangeDir Entity Dir
           | Move Entity Dir
           | Shoot Entity Dir

makeLenses ''Pos
makeLenses ''Entity
makeLenses ''Config



-- Combinators
type Step m a = ReaderT Config (WriterT [Event] m) a

config :: Monad m => Step m Config
config = ask

emit :: Monad m => Event -> Step m ()
emit evt = tell [evt]

env :: Monad m => Enemy -> Step m Config
env e = config >>= return . over enemies (delete e)

runStep :: Monad m => Step m a -> Config -> m (a, [Event])
runStep step = runWriterT . runReaderT step



-- Run
merge :: Monad m => Config -> [Event] -> m Config
merge conf evs = undefined

ended :: Config -> Bool
ended conf = undefined

runGame :: Monad m => Step m Config -> Config -> m Config
runGame step init =
  iterateUntilM ended (\c -> runStep step c >>= uncurry merge) init
   



-- Util
delete :: Eq a => a -> [a] -> [a]
delete deleted xs = [ x | x <- xs, x /= deleted ]




-- Exemple Prog
computer :: Monad m => Step m ()
computer = config >>= mapM_ (\e -> act e) . _enemies

act :: Monad m => Enemy -> Step m ()
act e = do
  c <- env e
  case _variant e of
    Spinner d -> emit $ spin d (_entity e) c
    Walker a b -> emit $ walk a b (_entity e) c

spin :: Dir -> Entity -> Config -> Event
spin d e c = ChangeDir e (succ d)

walk :: Pos -> Pos -> Entity -> Config -> Event
walk a b e c = undefined 
  

-- Prog
prog :: IO ()
prog = void $ runGame (computer >> config) initConf
  where initConf = undefined


