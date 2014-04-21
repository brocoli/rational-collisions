
module Base.Vector
  ( Vector(..)
  , sumVectors
  ) where

import Util.Util
  ( parallelAp
  )
import Base.Coordinate
  ( Coordinate
  )


type Vector = (Coordinate,Coordinate)


sumVectors :: Vector -> Vector -> Vector
sumVectors = parallelAp (+)
