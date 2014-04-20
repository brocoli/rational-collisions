
module Base.Vector
  ( Vector(..)
  , sumVectors
  ) where

import Base.Coordinate
  ( Coordinate
  )

import Util.Util
  ( parallelAp
  )


type Vector = (Coordinate, Coordinate)


sumVectors :: Vector -> Vector -> Vector
sumVectors = parallelAp (+)
