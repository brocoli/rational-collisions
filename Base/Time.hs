
module Base.Time
  ( Time(..)
  , TimeFraction(..)
  ) where

import Data.Ratio
  ( Rational
  )

data Time = MinusInfinity | Finite Rational | Infinity
  deriving(Ord,Eq,Show)

type TimeFraction = Rational
