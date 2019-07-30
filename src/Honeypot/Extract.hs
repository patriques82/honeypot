{-# LANGUAGE NoImplicitPrelude #-}

module Honeypot.Extract
  ( Extract
  , runExt
  , cell
  , up
  , down
  , left
  , right
  , until
  , countEmpty
  , notEmpty
  ) where

import           Honeypot.Prelude
import           Honeypot.Types

newtype Extract a = Ext { runExt :: Pos -> Matrix Cell -> a }

instance Functor Extract where
  fmap f (Ext g) = Ext $ \p m -> f (g p m)

instance Applicative Extract where
  pure x = Ext $ \p m -> x
  (Ext f) <*> (Ext x) = Ext $ \p m -> (f p m) (x p m)

instance Monad Extract where
  return = pure
  (Ext x) >>= f = Ext $ \p m ->
    runExt (f (x p m)) p m

cell :: Extract Cell
cell = Ext $ \(x,y) m -> m ! (x,y)

outOfBounds :: Pos -> Matrix Cell -> Bool
outOfBounds (x,y) m = undefined
  --x > xx || x < 0 || y > yy || y < 0


up :: Extract a -> Extract a
up (Ext f) = Ext $ \(x,y) m ->
  f (x,y-1) m

down :: Extract a -> Extract a
down (Ext f) = Ext $ \(x,y) m ->
  f (x,y+1) m

left :: Extract a -> Extract a
left (Ext f) = Ext $ \(x,y) m ->
  f (x-1,y) m

right :: Extract a -> Extract a
right (Ext f) = Ext $ \(x,y) m ->
  f (x+1,y) m

until :: Semigroup a => (Cell -> Bool) -> Extract a -> (Extract a -> Extract a) -> Extract a
until pred ext trans = do
  c <- cell
  if pred c
     then ext
     else (<>) <$> ext <*> trans (until pred ext trans)

countEmpty :: Cell -> Sum Int
countEmpty Empty = Sum 1
countEmpty _     = Sum 0

notEmpty :: Cell -> Bool
notEmpty Empty = False
notEmpty _     = True
