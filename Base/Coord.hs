
module Primitives.Coord
  ( Coord(..)
  , Position(..)
  ) where

import Data.Ratio
  ( Rational
  )

type Coord = Rational

type Position = (Coord, Coord)
