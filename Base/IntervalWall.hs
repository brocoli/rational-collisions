
module Base.IntervalWall
  ( IntervalWall(..)
  ) where

import Base.Coordinate
  ( Coordinate
  )

data IntervalWall = IntervalWall
  { position_ :: Coordinate
  , lean_     :: Ordering
  } deriving (Ord,Eq,Show)
