
module Base.Interval
  ( Interval(..)
  , newInterval
  , prettyInterval
  , prettyMaybeInterval
  ) where

import Base.Openess
  ( Openess(..)
  )

import Base.Coordinate
  ( Coordinate(..)
  )

import Base.IntervalWall
  ( IntervalWall(..)
  )

type Interval = (IntervalWall, IntervalWall)

newInterval :: Openess -> Coordinate -> Coordinate -> Maybe Interval
newInterval Open small big =
  if small < big
    then Just (IntervalWall small GT, IntervalWall big LT)
    else Nothing
newInterval Closed small big =
  if small <= big
    then Just (IntervalWall small EQ, IntervalWall big EQ)
    else Nothing

prettyInterval :: Interval -> String
prettyInterval (IntervalWall small smallL, IntervalWall big bigL) =
  concat [smallBracket, show small, ", ", show big, bigBracket]
    where smallBracket = case smallL of
                           LT -> "?"
                           EQ -> "["
                           GT -> "("
          bigBracket   = case bigL   of
                           LT -> ")"
                           EQ -> "]"
                           GT -> "?"

prettyMaybeInterval :: Maybe Interval -> String
prettyMaybeInterval (Just interval) = "Just " ++ prettyInterval interval
prettyMaybeInterval Nothing         = "Nothing"
