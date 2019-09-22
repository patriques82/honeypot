module Honeypot.Config.Util where

import           Honeypot.Prelude
import           Honeypot.Types

outOfBounds :: Dim -> Pos -> Bool
outOfBounds (P yy xx) (P y x) =
  x > xx || x < 1 || y > yy || y < 1
{-# INLINE outOfBounds #-}
