module Honeypot.Prelude
  ( Applicative (..)
  , Semigroup (..)
  , Bool (..)
  , Eq (..)
  , FilePath
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
  , Float
  , IO
  , (<$>)
  , ($)
  , ($!)
  , (.)
  , (||)
  , (&&)
  , (++)
  , (/)
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
  , fromIntegral
  , fromRational
  , head
  , id
  , last
  , not
  , null
  , otherwise
  , print
  , putStr
  , putStrLn
  , repeat
  , round
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
import           Prelude             (Float, Fractional, Num (..), Ord (..),
                                      Show (..), String, const, flip,
                                      fromIntegral, fromRational, id, round,
                                      subtract, undefined, ($), ($!), (.), (/))
import           System.IO           (FilePath, IO, print, putStr, putStrLn)
