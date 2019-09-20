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
  , elem
  , filter
  , find
  , flip
  , foldl'
  , foldr
  , fromMaybe
  , head
  , id
  , last
  , not
  , null
  , otherwise
  , repeat
  , subtract
  , tail
  , traverse
  , undefined
  , zip
  , zipWith
  ) where

import           Control.Applicative (Applicative (..))
import           Control.Monad       (Monad (..))
import           Data.Bool           (Bool (..), bool, not, otherwise, (&&),
                                      (||))
import           Data.Either         (Either (..))
import           Data.Eq             (Eq (..))
import           Data.Functor        (Functor (..), (<$>))
import           Data.Int            (Int)
import           Data.List           (any, elem, filter, find, foldl', foldr,
                                      head, last, null, repeat, tail, zip,
                                      zipWith, (++))
import           Data.Maybe          (Maybe (..), fromMaybe)
import           Data.Semigroup      (Semigroup (..), Sum (..))
import           Data.Traversable    (traverse)
import           Prelude             (Num (..), Ord (..), Show (..), String,
                                      const, flip, id, subtract, undefined, ($),
                                      ($!), (.))
import           System.IO           (IO)
