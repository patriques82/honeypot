{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Control.Monad.Reader
import qualified Data.Map             as M

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- Data Types
data Pos = Pos Int Int
  deriving (Eq, Ord)

type Dim = Pos

data Dir = Left | Up | Right | Down
  deriving (Eq, Ord, Enum, Bounded)

data Variant = Spinner Dir
             | Walker Pos Pos
  deriving Eq

data Event = ChangeDir Dir
           | Move Dir
           | Shoot
  deriving (Eq, Ord)




data Entity = Entity { _life     :: Integer
                     , _dir      :: Dir
                     , _shooting :: Bool
                     } deriving Eq

data Enemy = Enemy Entity Variant
  deriving Eq

type TEnemy = (Event, Enemy)

newtype Player = Player Entity

type TPlayer = (Event, Player)



data Env = Env { _dim       :: Dim
               , _obstacles :: [Pos]
               , _enemies   :: M.Map Pos Enemy
               , _player    :: (Pos, Player)
               }

data TEnv = TEnv Dim [Pos] (M.Map Pos TEnemy) (Pos, TPlayer)



-- Step Combinators

-- Similar to a State Monad with the difference that the state change is withheld
-- until the total step is runned so that intermediate steps doesnt know the
-- state changes that are occuring during the execution of the total step.

newtype Step a = Step (Reader Env a) -- hide constructor on export
  deriving (Functor, Applicative, Monad, MonadReader Env)


runStep :: Step a -> Env -> a
runStep (Step step) = runReader step


env' :: Step Env
env' = Step ask

dim' :: Step Dim
dim' = _dim <$> env'

obstacles' :: Step [Pos]
obstacles' = _obstacles <$> env'

-- TODO List of (Pos, Enemy)
enemies' :: Step (M.Map Pos Enemy)
enemies' = _enemies <$> env'

player' :: Step (Pos, Player)
player' = _player <$> env'


data Rules

-- internal
-- TODO parts of enemystep subsystem
enemyStep :: Rules -> Step (M.Map Pos TEnemy)
enemyStep = undefined -- logic

-- internal
playerStep :: Event -> Step (Pos, TPlayer)
playerStep event = ((,) event <$>) <$> player'

-- exported to user to create (StepT m TEnv)
transStep :: Rules -> Event -> Step TEnv
transStep r e = TEnv <$> dim'
                     <*> obstacles'
                     <*> enemyStep r
                     <*> playerStep e

-- exported
frames :: Step TEnv -> Env -> [Env]
frames step env = let tenv = runStep step env
                  in case resolve tenv of
                    Just e  -> e : frames step e
                    Nothing -> []



-- TODO parts of resolve subsystem
resolve :: TEnv -> Maybe Env
resolve tenv@(TEnv d o _ _) = do
  e' <- resolveEnemies tenv
  p' <- resolvePlayer tenv
  return (Env d o e' p')

resolveEnemies :: TEnv -> Maybe (M.Map Pos Enemy)
resolveEnemies = undefined

resolvePlayer :: TEnv -> Maybe (Pos, Player)
resolvePlayer = undefined



