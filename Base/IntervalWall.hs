
module Primitives.IntervalWall
  ( IntervalWall(..)
  , sumPosOpenSize
  ) where

import Primitives.Coord
  ( Coord
  )

import Primitives.OpenSize
  ( Openess(..)
  , OpenSize(..)
  )

data IntervalWall = IntervalWall
  { _IntervalWallPosition :: Coord
  , _IntervalWallLean     :: Ordering
  } deriving (Eq,Ord,Show)


sumPosOpenSize :: Coord -> OpenSize -> Maybe IntervalWall
sumPosOpenSize center size =
  case compare magnitude 0 of
    LT -> case openess of
            Closed -> Just $ IntervalWall position EQ
            Open   -> Just $ IntervalWall position GT
    EQ -> Nothing
    GT -> case openess of
            Open   -> Just $ IntervalWall position LT
            Closed -> Just $ IntervalWall position EQ
  where magnitude = _OpenSizeMagnitude size
        position  = center + magnitude
        openess   = _OpenSizeOpeness size
