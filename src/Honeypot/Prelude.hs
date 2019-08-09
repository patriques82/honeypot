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
  , Num (..)
  , Ord (..)
  , Sum (..)
  , String
  , Int
  , IO
  , (<$>)
  , ($)
  , ($!)
  , (.)
  , (||)
  , (&&)
  , (++)
  , any
  , bool
  , const
  , filter
  , find
  , flip
  , foldl'
  , head
  , id
  , last
  , not
  , otherwise
  , repeat
  , subtract
  , tail
  , traverse
  , undefined
  , zip
  ) where

import           Control.Applicative (Applicative (..))
import           Control.Monad       (Monad (..))
import           Data.Bool           (Bool (..), bool, not, otherwise, (&&),
                                      (||))
import           Data.Either         (Either (..))
import           Data.Eq             (Eq (..))
import           Data.Functor        (Functor (..), (<$>))
import           Data.Int            (Int)
import           Data.List           (any, filter, find, foldl', head, last,
                                      repeat, tail, zip, (++))
import           Data.Maybe          (Maybe (..))
import           Data.Semigroup      (Semigroup (..), Sum (..))
import           Data.Traversable    (traverse)
import           Prelude             (Num (..), Ord (..), Show (..), String,
                                      const, flip, id, subtract, undefined, ($),
                                      ($!), (.))
import           System.IO           (IO)
