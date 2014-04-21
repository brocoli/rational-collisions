
module Util.Pretty
  ( prettyInterval
  , prettyAABB
  , prettyfyMaybe
  ) where

import Base.IntervalWall
  ( IntervalWall(..)
  )
import Base.Interval
  ( Interval
  )
import Primitive.AABB
  ( AABB
  )

import Data.Ratio

prettyInterval :: Interval -> String
prettyInterval (IntervalWall smallL small, IntervalWall bigL big) =
  concat [smallBracket, show small, ", ", show big, bigBracket]
    where smallBracket = case smallL of
                           LT -> "?"
                           EQ -> "["
                           GT -> "("
          bigBracket   = case bigL   of
                           LT -> ")"
                           EQ -> "]"
                           GT -> "?"

prettyAABB :: AABB -> String
prettyAABB (xInterval,yInterval) =
  concat [prettyInterval xInterval, " x ", prettyInterval yInterval]

prettyfyMaybe :: (a -> String) -> Maybe a -> String
prettyfyMaybe pretty (Just value) = "Just " ++ pretty value
prettyfyMaybe _      Nothing      = "Nothing"
