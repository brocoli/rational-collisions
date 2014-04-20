
module Primitive.AABB
  ( AABB(..)
  , newAABB
  ) where

import Base.Openess
  ( Openess(..)
  )

import Base.Coordinate
  ( Coordinate(..)
  )

import Base.Interval
  ( Interval(..)
  , newInterval
  )


type AABB = (Interval, Interval)


newAABB :: Openess -> Coordinate -> Coordinate
                   -> Coordinate -> Coordinate -> Maybe AABB
newAABB openess left top right bottom = do
  xInterval <- newInterval openess left right
  yInterval <- newInterval openess top  bottom
  return (xInterval, yInterval)
