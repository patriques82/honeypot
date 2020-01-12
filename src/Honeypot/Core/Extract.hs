{-# LANGUAGE BangPatterns #-}

module Honeypot.Core.Extract
  ( Extract
  , runExt
  , cell
  , upE
  , downE
  , leftE
  , rightE
  , until
  , countEmpty
  , notEmpty
  ) where

import           Data.Matrix      (safeGet, (!))
import           Honeypot.Prelude
import           Honeypot.Types
import           Lens.Simple      ((^.))

newtype Extract a = Ext { runExt :: Pos -> Env -> a }

instance Functor Extract where
  fmap f (Ext g) = Ext $ \p e -> f (g p e)

instance Applicative Extract where
  pure x = Ext $ \_ _ -> x
  (Ext f) <*> (Ext x) = Ext $ \p e -> f p e (x p e)

instance Monad Extract where
  return = pure
  (Ext x) >>= f = Ext $ \p e ->
    runExt (f (x p e)) p e

cell :: Extract Cell
cell = Ext $ \p@(P y x) e ->
  let es = e ^. enemies
      !ts = unBoard $ e ^. terrain
      enemy = const Enemy <$> find ((==) p . current) es
      block = bool Nothing (Just Block) (ts ! (y,x))
   in case ts !? p of
        Nothing -> Wall
        _       -> fromMaybe Empty (enemy <> block)

shift :: Extract a -> (Pos -> Pos) -> Extract a
shift (Ext f) g = Ext $ \p e -> f (g p) e

upE :: Extract a -> Extract a
upE ext = shift ext (forward North)

downE :: Extract a -> Extract a
downE ext = shift ext (forward South)

leftE :: Extract a -> Extract a
leftE ext = shift ext (forward West)

rightE :: Extract a -> Extract a
rightE ext = shift ext (forward East)

until :: Semigroup a => (Cell -> Bool) -> Extract a -> (Extract a -> Extract a) -> Extract a
until pred ext trans = do
  c <- cell
  if pred c
     then ext
     else (<>) <$> ext <*> (trans $! until pred ext trans)

countEmpty :: Cell -> Sum Int
countEmpty Empty = Sum 1
countEmpty _     = Sum 0

notEmpty :: Cell -> Bool
notEmpty Empty = False
notEmpty _     = True
