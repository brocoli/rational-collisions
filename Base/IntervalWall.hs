
module Base.IntervalWall
  ( IntervalWall(..)
  , translateIntervalWall
  ) where

import Base.Coordinate
  ( Coordinate
  )

data IntervalWall = IntervalWall
  { lean_     :: Ordering
  , position_ :: Coordinate
  } deriving (Ord,Eq,Show)

translateIntervalWall :: Coordinate -> IntervalWall -> IntervalWall
translateIntervalWall trans (IntervalWall lean pos) =
  IntervalWall lean $ pos + trans
