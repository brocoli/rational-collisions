
module Primitive.AABB
  ( AABB(..)
  , newAABB
  , translateAABB
  , minkowskySumAABBs
  ) where

import Util.Util
  ( mapTuple
  , parallelAp
  , swapCross
  , (.:)
  )
import Base.Openess
  ( Openess(..)
  )
import Base.Coordinate
  ( Coordinate(..)
  )
import Base.Vector
  ( Vector(..)
  )
import Base.Interval
  ( Interval(..)
  , newInterval
  , translateInterval
  , minkowskySumIntervals
  )


type AABB = (Interval,Interval)


newAABB :: Openess -> Coordinate -> Coordinate
                   -> Coordinate -> Coordinate -> Maybe AABB
newAABB openess left top right bottom = do
  xInterval <- newInterval openess left right
  yInterval <- newInterval openess top  bottom
  return (xInterval,yInterval)

translateAABB :: Vector -> AABB -> AABB
translateAABB = parallelAp translateInterval

minkowskySumAABBs :: AABB -> AABB -> AABB
minkowskySumAABBs =
  ((mapTuple $ uncurry minkowskySumIntervals) . swapCross) .: (,)
