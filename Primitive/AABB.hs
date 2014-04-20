
module Primitive.AABB
  ( AABB(..)
  , newAABB
  , translateAABB
  ) where

import Util.Util
  ( parallelAp
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
  )


type AABB = (Interval, Interval)


newAABB :: Openess -> Coordinate -> Coordinate
                   -> Coordinate -> Coordinate -> Maybe AABB
newAABB openess left top right bottom = do
  xInterval <- newInterval openess left right
  yInterval <- newInterval openess top  bottom
  return (xInterval, yInterval)

translateAABB :: Vector -> AABB -> AABB
translateAABB = parallelAp translateInterval
