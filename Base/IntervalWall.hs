
module Base.IntervalWall
  ( IntervalWall(..)
  , translateIntervalWall
  ) where

import Base.Coordinate
  ( Coordinate
  )

data IntervalWall = IntervalWall
  { position_ :: Coordinate
  , lean_     :: Ordering
  } deriving (Ord,Eq,Show)

translateIntervalWall :: Coordinate -> IntervalWall -> IntervalWall
translateIntervalWall trans (IntervalWall pos lean) =
  IntervalWall (pos+trans) lean
