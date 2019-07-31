{-# LANGUAGE NoImplicitPrelude #-}

module Honeypot.Prelude
  ( module Data.Matrix
  , Applicative (..)
  , Semigroup (..)
  , Bool (..)
  , Functor (..)
  , Monad (..)
  , Maybe (..)
  , Show (..)
  , Num (..)
  , Ord (..)
  , Sum (..)
  , Int
  , IO
  , Map
  , filter
  , foldrWithKey'
  , insert
  , lookup
  , singleton
  , undefined
  , const
  , id
  , subtract
  , flip
  , (<$>)
  , ($)
  , (.)
  , (||)
  , not
  ) where

import           Control.Applicative (Applicative (..))
import           Control.Monad       (Monad (..))
import           Data.Eq             (Eq (..))
import           Data.Functor        (Functor (..), (<$>))
import           Data.Int            (Int)
import           Data.Map            (Map, filter, foldrWithKey', insert,
                                      lookup, singleton)
import           Data.Matrix
import           Data.Maybe          (Maybe (..))
import           Data.Semigroup      (Semigroup (..), Sum (..))
import           Prelude             (Bool (..), Num (..), Ord (..), Show (..),
                                      const, flip, id, not, subtract, undefined,
                                      ($), (.), (||))
import           System.IO           (IO)
