{-# LANGUAGE NoImplicitPrelude #-}

module Honeypot.Extract
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

import           Data.Matrix
import           Honeypot.Prelude
import           Honeypot.Types

newtype Extract a = Ext { runExt :: Pos -> Env -> a }

instance Functor Extract where
  fmap f (Ext g) = Ext $ \p e -> f (g p e)

instance Applicative Extract where
  pure x = Ext $ \_ _ -> x
  (Ext f) <*> (Ext x) = Ext $ \p e -> (f p e) (x p e)

instance Monad Extract where
  return = pure
  (Ext x) >>= f = Ext $ \p e ->
    runExt (f (x p e)) p e

cell :: Extract Cell
cell = Ext $ \p e ->
  let enemy = const Enemy <$> find ((==) p . current) (enemies e)
      block = bool Nothing (Just Block) (terrain (board e) ! p)
   in if outOfBounds (dim (board e)) p
         then Wall
         else case enemy <> block of
                Nothing -> Empty
                Just x  -> x

upE :: Extract a -> Extract a
upE (Ext f) = Ext $ \(y,x) m ->
  f (y-1,x) m

downE :: Extract a -> Extract a
downE (Ext f) = Ext $ \(y,x) m ->
  f (y+1,x) m

leftE :: Extract a -> Extract a
leftE (Ext f) = Ext $ \(y,x) m ->
  f (y,x-1) m

rightE :: Extract a -> Extract a
rightE (Ext f) = Ext $ \(y,x) m ->
  f (y,x+1) m

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
