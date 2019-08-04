{-# LANGUAGE NoImplicitPrelude #-}

module Honeypot.Prelude
  ( Applicative (..)
  , Semigroup (..)
  , Bool (..)
  , Eq (..)
  , Functor (..)
  , Monad (..)
  , Maybe (..)
  , Either (..)
  , Show (..)
  , Set (..)
  , Num (..)
  , Ord (..)
  , Sum (..)
  , String
  , Int
  , IO
  , Map
  , traverse
  , filter
  , fromList
  , member
  , map
  , insert
  , lookup
  , singleton
  , head -- rewrite
  , tail -- rewrite safe
  , last -- rewrite
  , zip
  , foldl'
  , repeat
  , (++)
  , undefined
  , otherwise
  , const
  , id
  , subtract
  , flip
  , find
  , (<$>)
  , ($)
  , (.)
  , (||)
  , (&&)
  , not
  ) where

import           Control.Applicative (Applicative (..))
import           Control.Monad       (Monad (..))
import           Data.Bool           (Bool (..), bool, not, otherwise, (&&),
                                      (||))
import           Data.Either         (Either (..))
import           Data.Eq             (Eq (..))
import           Data.Functor        (Functor (..), (<$>))
import           Data.Int            (Int)
import           Data.List           (find, foldl', head, last, repeat, tail,
                                      zip, (++))
import           Data.Map            (Map, insert, lookup, singleton)
import           Data.Maybe          (Maybe (..))
import           Data.Semigroup      (Semigroup (..), Sum (..))
import           Data.Set            (Set (..), filter, fromList, map, member)
import           Data.Traversable    (traverse)
import           Prelude             (Num (..), Ord (..), Show (..), String,
                                      const, flip, id, subtract, undefined, ($),
                                      (.))
import           System.IO           (IO)
