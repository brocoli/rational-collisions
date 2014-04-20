
module Base.Interval
  ( Interval(..)
  , newInterval
  , translateInterval
  ) where

import Util.Util
  ( mapTuple
  , (>|<)
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


type Interval = (IntervalWall,IntervalWall)


makeInterval :: (Ordering,Ordering) -> (Coordinate,Coordinate) -> Interval
makeInterval = IntervalWall >|< IntervalWall

newInterval :: Openess -> Coordinate -> Coordinate -> Maybe Interval
newInterval openess small big
  | small `comp` big = Just $ makeInterval leans (small,big)
  | otherwise        = Nothing
  where comp  = case openess of
                  Open   -> (<)  -- 0-width open intervals are not allowed
                  Closed -> (<=) -- since (x,GT) > (x,LT) always
        leans = case openess of
                  Open   -> (GT,LT)
                  Closed -> (EQ,EQ)

translateInterval :: Coordinate -> Interval -> Interval
translateInterval = mapTuple . translateIntervalWall
