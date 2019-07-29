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

newtype Extract a = Ext { runExt :: Dim -> Pos -> Map Pos Cell -> a }

instance Functor Extract where
  fmap f (Ext g) = Ext $ \d p m -> f (g d p m)

instance Applicative Extract where
  pure x = Ext $ \d p m -> x
  (Ext f) <*> (Ext x) = Ext $ \d p m -> (f d p m) (x d p m)

instance Monad Extract where
  return = pure
  (Ext x) >>= f = Ext $ \d p m ->
    runExt (f (x d p m)) d p m

cell :: Extract Cell
cell = Ext $ \d p m ->
  if not (outOfBounds d p)
     then case lookup p m of
       Just o  -> o
       Nothing -> Empty
     else Wall

outOfBounds :: Dim -> Pos -> Bool
outOfBounds (xx,yy) (x,y) =
  x > xx || x < 0 || y > yy || y < 0


up :: Extract a -> Extract a
up (Ext f) = Ext $ \d (x,y) m ->
  f d (x,y-1) m

down :: Extract a -> Extract a
down (Ext f) = Ext $ \d (x,y) m ->
  f d (x,y+1) m

left :: Extract a -> Extract a
left (Ext f) = Ext $ \d (x,y) m ->
  f d (x-1,y) m

right :: Extract a -> Extract a
right (Ext f) = Ext $ \d (x,y) m ->
  f d (x+1,y) m

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
