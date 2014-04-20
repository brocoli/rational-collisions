
module Primitive.AABB
  ( AABB(..)
  , AABBVoronoi(..)
  , newAABB
  , prettyAABB
  , prettyMaybeAABB
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
  , prettyInterval
  )


type AABB = (Interval, Interval)

type AABBVoronoi = (Ordering, Ordering)


newAABB :: Openess -> Coordinate -> Coordinate
                   -> Coordinate -> Coordinate -> Maybe AABB
newAABB openess left top right bottom = do
  xInterval <- newInterval openess left right
  yInterval <- newInterval openess top  bottom
  return (xInterval, yInterval)


prettyAABB :: AABB -> String
prettyAABB (xInterval, yInterval) =
  concat [prettyInterval xInterval, " x ", prettyInterval yInterval]

prettyMaybeAABB :: Maybe AABB -> String
prettyMaybeAABB (Just aabb) = "Just " ++ prettyAABB aabb
prettyMaybeAABB Nothing     = "Nothing"
