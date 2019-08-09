module Honeypot.Config.Util where

import           Honeypot.Prelude
import           Honeypot.Types

outOfBounds :: Dim -> Pos -> Bool
outOfBounds (yy,xx) (y,x) =
  x > xx || x < 1 || y > yy || y < 1
