
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
  } deriving (Eq,Show)

-- this Ord instance could be avoided if the IntervalWall record had its
-- fields reversed, but then it would become harder to partially apply
-- the IntervalWall constructor.
instance Ord IntervalWall where
  IntervalWall lean1 pos1 `compare` IntervalWall lean2 pos2 =
    let mainComparison = pos1 `compare` pos2 in
      if mainComparison == EQ
        then lean1 `compare` lean2
        else mainComparison

translateIntervalWall :: Coordinate -> IntervalWall -> IntervalWall
translateIntervalWall trans (IntervalWall lean pos) =
  IntervalWall lean $ pos + trans
