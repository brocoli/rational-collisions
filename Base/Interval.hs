
module Base.Interval
  ( Interval(..)
  , newInterval
  , translateInterval
  , minkowskySumIntervals
  , hasZero
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

hasZero :: Interval -> Bool
hasZero (small, big) = let zero = IntervalWall EQ 0 in
                         small <= zero && zero <= big

minkowskySumIntervals :: Interval -> Interval -> Interval
minkowskySumIntervals (IntervalWall sl1 sp1, IntervalWall bl1 bp1)
                      (IntervalWall sl2 sp2, IntervalWall bl2 bp2) =
  (IntervalWall leanSmall (centerP-radP), IntervalWall leanBig (centerP+radP))
  where centerP   = ((sp2+bp2)-(sp1+bp1)) / 2
        radP      = ((bp2-sp2)+(bp1-sp1)) / 2
        leanSmall = if sl1 == EQ && sl2 == EQ
                      then EQ
                      else GT
        leanBig   = if bl1 == EQ && bl2 == EQ
                      then EQ
                      else LT
