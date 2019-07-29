{-# LANGUAGE NoImplicitPrelude #-}

module Honeypot.Prelude
  ( Applicative (..)
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
  , lookup
  , undefined
  , (<$>)
  , ($)
  , (.)
  , (||)
  , not
  ) where

import           Control.Applicative  (Applicative (..))
import           Control.Monad        (Monad (..))
import           Control.Monad.Reader (MonadReader, Reader, ask, runReader)
import           Data.Eq              (Eq (..))
import           Data.Functor         (Functor (..), (<$>))
import           Data.Int             (Int)
import           Data.Map             (Map, lookup)
import           Data.Maybe           (Maybe (..))
import           Data.Semigroup       (Semigroup (..), Sum (..))
import           Prelude              (Bool (..), Num (..), Ord (..), Show (..),
                                       not, undefined, ($), (.), (||))
import           System.IO            (IO)
