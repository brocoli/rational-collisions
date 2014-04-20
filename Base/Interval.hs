
module Base.Interval
  ( Interval(..)
  , newInterval
  , translateInterval
  ) where

import Util.Util
  ( mapTuple
  )

import Base.Openess
  ( Openess(..)
  )

import Base.Coordinate
  ( Coordinate(..)
  )

import Base.IntervalWall
  ( IntervalWall(..)
  , translateIntervalWall
  )


type Interval = (IntervalWall, IntervalWall)


makeOpenInterval :: Coordinate -> Coordinate -> Interval
makeOpenInterval small big = (IntervalWall small GT, IntervalWall big LT)

makeClosedInterval :: Coordinate -> Coordinate -> Interval
makeClosedInterval small big = (IntervalWall small EQ, IntervalWall big EQ)
-- makeClosedInterval = ((.).(.)) (join (***) $ flip IntervalWall EQ) (,)

newInterval :: Openess -> Coordinate -> Coordinate -> Maybe Interval
newInterval openess small big
  | small `comp` big = Just $ makeInterval small big
  | otherwise        = Nothing
  where (makeInterval, comp) =
          case openess of
            Open   -> (  makeOpenInterval, (<) )
            Closed -> (makeClosedInterval, (<=))

translateInterval :: Coordinate -> Interval -> Interval
translateInterval = mapTuple . translateIntervalWall
